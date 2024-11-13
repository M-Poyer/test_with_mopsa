(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Desugar accesses to aggregates into accesses to scalar arrays using
    pointer arithmetics.

    This domains translates subscript accesses `a[i]` and field accesses `s.f`
    into dereferences of scalar pointers. This is useful for low-level memory
    abstractions to handle the full C language.
*)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Stubs.Ast
open Common.Base
open Common.Points_to


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.memory.aggregates"
    end)

  let dependencies = []

  let checks = []


  (** {2 Command-line options} *)
  (** ======================== *)

  (** Size threshold (in bytes) for using memset to initialize memory blocks
      instead of a sequence of assignments *)
  let opt_init_memset_threshold = ref 50

  let () = register_domain_option name {
      key  = "-c-init-memset-threshold";
      doc  = " Size threshold (in bytes) for using memset to initialize memory blocks instead of a sequence of assignments";
      spec = ArgExt.Set_int opt_init_memset_threshold;
      category = "C";
      default = string_of_int !opt_init_memset_threshold;
    }


  (** {2 Initialization procedure} *)
  (** ============================ *)

  let init _ _ flow = None


  (** {2 Syntactic simplifications} *)
  (** ============================= *)

  (** a[i] -> *(a + i) *)
  let mk_lowlevel_subscript_access a i t range =
    mk_c_deref (mk_binop a O_plus i ~etyp:(pointer_type t) range) range

  (** 𝔼⟦ &(p->f) ⟧ -> ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
  let mk_lowlevel_field_address p i t range =
    let st = under_type p.etyp in
    let align = mk_int (align_byte st i) range in
    mk_c_cast
      (mk_binop
         (mk_c_cast p (pointer_type s8) range)
         O_plus
         align
         ~etyp:(pointer_type s8)
         range
      )
      (pointer_type t)
      range

  let pp_record_field fmt f =
    Format.fprintf fmt "%s@+%d,%d:%a" f.c_field_name f.c_field_offset f.c_field_bit_offset pp_typ f.c_field_type

  let pp_record_fields fmt fields =
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_record_field) fmt fields

  let locate_bitfield_cell field record_typ flow =
    let typ =
      match remove_typedef_qual field.c_field_type with
      | T_c_bitfield(t, _) -> t
      | _ -> assert false
    in
    let size = 8 * (sizeof_type typ flow |> Z.to_int) in
    let record =
      match remove_typedef_qual record_typ with
      | T_c_record r -> r
      | _ -> assert false
    in
    let rec iter = function
      | [] -> assert false
      | field' :: tl ->
        match remove_typedef_qual field'.c_field_type with
        | T_c_bitfield(typ', width')
          when compare_typ (remove_typedef_qual typ) (remove_typedef_qual typ') = 0 ->
          let rec iter2 acc_width = function
            | [] -> assert false
            | field'' :: tl ->
              if field''.c_field_name = field.c_field_name then
                field'.c_field_offset, acc_width
              else
                match remove_typedef_qual field''.c_field_type with
                | T_c_bitfield(typ'', width'')
                  when compare_typ (remove_typedef_qual typ) (remove_typedef_qual typ'') = 0 ->
                  if width'' + acc_width < size then
                    iter2 (width'' + acc_width) tl
                  else
                    iter tl

                | _ -> iter tl
          in
          iter2 0 (field' :: tl)

        | _ -> iter tl
    in
    let cell_offset, bitfield_offset = iter record.c_record_fields in
    cell_offset, bitfield_offset, typ

  (** Transform an access to a bitfield into an access to the container scalar with appropriate bitwise operations *)
  let mk_lowlevel_bitfield_access p field range flow =
    (* Lval of the container scalar: *( field.type* )(( u8* )p + field.offset) *)
    let cell_offset, bitfield_offset, t = locate_bitfield_cell field (under_type p.etyp) flow in
    let lval = mk_c_deref (mk_c_cast (add (mk_c_cast p (pointer_type u8) range) (mk_int cell_offset range) ~typ:(pointer_type u8) range) (pointer_type t) range) range in
    (* Mask of bitfield: 11..<WIDTH times>..1100..<OFFSET times>00 *)
    let width = match remove_typedef_qual field.c_field_type with T_c_bitfield(_, w) -> w | _ -> assert false in
    let mask = Z.shift_left (Z.pow (Z.of_int 2) width |> Z.pred) bitfield_offset in
    (* Apply the mask on the cell and shift the result to the right *)
    mk_binop
      (mk_binop lval O_bit_and (mk_z mask range) ~etyp:t range)
      O_bit_rshift
      (mk_int bitfield_offset range) ~etyp:t range

  (** 𝔼⟦ p->f ⟧ -> *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
  let mk_lowlevel_arrow_access p i t range flow =
    let t' = under_type p.etyp in
    let field = find_c_record_field i t' in
    if is_c_bitfield field.c_field_type then
      mk_lowlevel_bitfield_access p field range flow
    else
      mk_c_deref (mk_lowlevel_field_address p i t range) range

  (** s.f -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let mk_lowlevel_member_access s i t range =
    let p = mk_c_address_of s range in
    mk_lowlevel_arrow_access p i t range


  (** {2 Abstract transformers} *)
  (** ========================= *)

  

  let are_all_bitfields_of_same_cell_initialized inits fields flow =
    let hd = List.hd fields in
    let btyp0 =
      match remove_typedef_qual hd.c_field_type with
      | T_c_bitfield(btyp, _) -> btyp
      | _ -> assert false
    in
    let size = 8 * (sizeof_type btyp0 flow |> Z.to_int) in
    let rec iter acc_width inits = function
      | [] -> true
      | field :: tlfields ->
        match remove_typedef_qual field.c_field_type with
        | T_c_bitfield(btyp, bwidth)
          when compare_typ (remove_typedef_qual btyp0) (remove_typedef_qual btyp) = 0 &&
               acc_width + bwidth <= size ->
          begin
            match inits with
            | Some _ :: tlinit -> iter (acc_width + bwidth) tlinit tlfields
            | _ -> false
          end
        | _ -> true
    in
    iter 0 inits fields


  (** The following functions flatten the initialization expression
      into a list of scalar initializations *)
  let rec flatten_init base init offset typ range flow =
    if is_c_scalar_type typ then flatten_scalar_init base init offset typ range flow else
    if is_c_array_type typ  then flatten_array_init base init offset typ range flow else
    if is_c_record_type typ then flatten_record_init base init offset typ range flow
    else panic_at ~loc:__LOC__ range
        "init %a of type %a not supported"
        (OptionExt.print Pp.pp_c_init) init pp_typ typ

  and flatten_scalar_init base init offset typ range flow =
    match init with
    | None                 -> [],[(None, Z.one, offset, typ)]
    | Some (C_init_expr e) -> [(e, offset, typ)],[]
    (* a scalar initializer can be optionnaly enclosed in braces *)
    | Some (C_init_list ([C_init_expr e], _)) -> [(e, offset, typ)],[]
    | Some init -> panic_at range "unsupported scalar initializer %a for type %a" Pp.pp_c_init init pp_typ typ;

  and flatten_array_init base init offset typ range flow =
    if is_c_no_length_array_type typ
    || is_c_variable_length_array_type typ
    then [],[] else

    let n = get_array_constant_length typ in
    let under_typ = under_array_type typ in
    match init with
    | None ->
      if is_c_scalar_type under_typ then
        [],[(None,n,offset,under_typ)]
      else
        let nn = Z.mul n (sizeof_type under_typ flow) in
        [],[(None,nn,offset,u8)]

    (* a string literal can be optionally enclosed in braces *)
    | Some (C_init_list ([C_init_expr {ekind = E_constant(C_c_string _); } as i], _))
         when is_c_int_array_type typ ->
       flatten_array_init base (Some i) offset typ range flow

    | Some (C_init_list (l, filler)) ->
      let rec aux i l acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ flow)) in
        if Z.equal (Z.of_int i) n
        then acc1, acc2
        else
          match l with
          | elem::rest ->
            let l1,l2 = flatten_init base (Some elem) o under_typ range flow in
            aux (i+1) rest (List.rev_append l1 acc1) (List.rev_append l2 acc2)
          | [] ->
            let remain = Z.sub n (Z.of_int i) in
            match filler with
            | None ->
              if is_c_scalar_type under_typ then
                acc1, (None,remain,o,under_typ)::acc2
              else
                let nn = Z.mul remain (sizeof_type under_typ flow) in
                acc1, (None,nn,o,u8)::acc2

            | Some (C_init_list([], Some (C_init_expr e)))
            | Some (C_init_expr e) ->
              acc1, (Some e, remain, o,under_typ)::acc2

            | Some (C_init_list(l, None)) ->
              aux i [C_init_list (l, None)] acc1 acc2

            | Some x ->
              panic_at range "initialization filler %a not supported" Pp.pp_c_init x
      in
      let l1,l2 = aux 0 l [] [] in
      List.rev l1, List.rev l2

    | Some (C_init_expr {ekind = E_constant(C_c_string (s, C_char_ascii)); etyp = t; erange}) ->
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ flow)) in
        if Z.equal (Z.of_int i) n
        then acc1, acc2

        else if i < String.length s
        then aux (i+1) ((mk_c_character (String.get s i) erange under_typ, o, under_typ)::acc1) acc2

        else if i = String.length s
        then aux (i+1) ((mk_c_character (char_of_int 0) erange under_typ, o, under_typ)::acc1) acc2

        else acc1,(None,Z.sub n (Z.of_int i), o, s8)::acc2
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    (* wide strings *)
    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _)); etyp = t; erange}) ->
      let char_size = sizeof_type under_typ flow in
      let nchar_size = Z.to_int char_size in
      let len = String.length s / nchar_size in
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) char_size) in
        if Z.equal (Z.of_int i) n
        then acc1,acc2

        else if i < len
        then aux (i+1) ((mk_c_multibyte_integer s (i * nchar_size) under_typ flow erange, o, under_typ)::acc1) acc2

        else if i = len
        then aux (i+1) ((mk_zero ~typ:under_typ erange, o, under_typ)::acc1) acc2

        else acc1,(None,Z.sub n (Z.of_int i), o, s8)::acc2
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    | Some (Ast.C_init_expr e) ->
      let rec aux i acc1 acc2 =
        let o = Z.add offset (Z.mul (Z.of_int i) (sizeof_type under_typ flow)) in
        if Z.equal (Z.of_int i) n
        then acc1,acc2
        else
          let init' = Some (C_init_expr (mk_lowlevel_subscript_access e (mk_int i e.erange) under_typ e.erange)) in
          let l1,l2 = flatten_init base init' o under_typ range flow in
          aux (i+1) (List.rev_append l1 acc1) (List.rev_append l2 acc2)
      in
      let l1,l2 = aux 0 [] [] in
      List.rev l1, List.rev l2

    | _ -> panic_at range ~loc:__LOC__
             "flatten_array_init: %a is not supported"
             Pp.pp_c_init (OptionExt.none_to_exn init)

  and flatten_record_init base init offset typ range flow =
    match remove_typedef_qual typ with
    | T_c_record{c_record_kind = C_struct} -> flatten_struct_init base init offset typ range flow
    | T_c_record{c_record_kind = C_union}  -> flatten_union_init  base init offset typ range flow
    | _ -> assert false

  and flatten_struct_init base init offset typ range flow =
    let fields =
      match remove_typedef_qual typ with
      | T_c_record{c_record_fields} -> c_record_fields
      | t -> panic_at ~loc:__LOC__ range "type %a is not a record" pp_typ t
    in
    match init with
    | None ->
      let rec aux acc1 acc2 = function
        | [] -> acc1,acc2
        | field :: tl ->
          let o = Z.add offset (Z.of_int field.c_field_offset) in
          let l1, l2, tl =
            match remove_typedef_qual field.c_field_type with
            | T_c_bitfield(btyp, bwidth) ->
              let l1, l2 = flatten_init base None o btyp range flow in
              let size = 8 * (Z.to_int @@ sizeof_type btyp flow) in
              let rec discard_same_scalar acc = function
                | [] -> []
                | f::tl as l ->
                  match remove_typedef_qual f.c_field_type with
                  | T_c_bitfield(btyp', bwidth')
                    when compare_typ (remove_typedef_qual btyp) (remove_typedef_qual btyp') = 0 &&
                         acc + bwidth' <= size ->
                    discard_same_scalar (acc + bwidth') tl

                  | _ -> l
              in
              let tl = discard_same_scalar bwidth tl in
              l1, l2, tl
            | _ ->
              let l1,l2 = flatten_init base None o field.c_field_type range flow in
              l1, l2, tl
          in
          aux (List.rev_append l1 acc1) (List.rev_append l2 acc2) tl
      in
      let l1,l2 = aux [] [] fields in
      List.rev l1, List.rev l2

    | Some (C_init_list(l, None)) ->
      let rec aux l acc1 acc2 fields =
        match fields with
        | [] -> acc1,acc2
        | field :: tl ->
          let o = Z.add offset (Z.of_int field.c_field_offset) in
          let l1, l2, tl, tll =
            match remove_typedef_qual field.c_field_type with
            | T_c_bitfield(btyp, _) ->
              let size = 8 * (sizeof_type btyp flow |> Z.to_int) in
              (* Collect all bitfields in the same cell *)
              let rec collect_bitfields acc_width inits = function
                | [] -> [], inits, []
                | field' :: tlfields ->
                  match remove_typedef_qual field'.c_field_type with
                  | T_c_bitfield(btyp', bwidth')
                    when compare_typ (remove_typedef_qual btyp) (remove_typedef_qual btyp') = 0 &&
                         acc_width + bwidth' <= size ->
                    let init', tlinit =
                      match inits with
                      | C_init_expr e :: tlinit -> e, tlinit
                      | C_init_list ([C_init_expr e], _) :: tlinit -> e, tlinit
                      | _ -> panic "aggregate: unexpected bitfield initializers: %a"
                               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") Pp.pp_c_init) inits
                    in
                    let bitfields, tlinits, tlfields = collect_bitfields (acc_width + bwidth') tlinit tlfields in
                    (acc_width, bwidth', init') :: bitfields, tlinits, tlfields
                  | _ -> [], inits, field'::tlfields
              in
              let bitfields, tll, tlfields = collect_bitfields 0 l fields in
              let mk_masked_init (offset, width, init) =
                let mask = mk_z (Z.pow (Z.of_int 2) width |> Z.pred) range in
                mk_binop (mk_binop init O_bit_and mask ~etyp:init.etyp range) O_bit_lshift (mk_int offset range) ~etyp:btyp range
              in
              let init' =
                List.fold_left (fun acc bitfield -> mk_binop acc O_log_or (mk_masked_init bitfield) ~etyp:btyp range)
                  (mk_masked_init @@ List.hd bitfields) (List.tl bitfields)
              in
              [init', o, btyp], [], tlfields, tll

            (* Non-bitfield case *)
            | _ ->
              let init, tll =
                match l with
                | [] -> None, []
                | init::tll -> Some init, tll
              in
              let l1', l2' = flatten_init base init o field.c_field_type range flow in
              l1', l2', tl, tll
          in
          aux tll (List.rev_append l1 acc1) (List.rev_append l2 acc2) tl
      in
      let l1,l2 = aux l [] [] fields in
      List.rev l1, List.rev l2

    | Some (C_init_expr e) when is_c_record_type e.etyp ->
      [(e,offset,typ)],[]

    | _ -> assert false

  and flatten_union_init base init offset typ range flow =
    let fields =
      match remove_typedef_qual typ with
      | T_c_record{c_record_fields} -> c_record_fields
      | t -> panic_at ~loc:__LOC__ range "type %a is not a record" pp_typ t
    in
    match init with
    | None ->
      let field = List.hd fields in
      flatten_init base init offset field.c_field_type range flow

    | Some (C_init_list([init], None)) ->
      let rec match_init_list_with_typ l typ =
        match remove_typedef_qual typ with
        | T_c_array(typ', _) ->
          List.for_all (fun init' -> match_init_with_typ init' typ') l

        | T_c_record r when r.c_record_kind = C_struct && List.length l <= List.length r.c_record_fields ->
          List.for_all (fun (i, init') ->
              let field = find_c_record_field i typ in
              match_init_with_typ init' field.c_field_type
            ) (List.mapi (fun i init' -> i, init') l)

        | _ -> false

      and match_init_with_typ init typ =
        match init with
        | C_init_expr e ->
          let typ' =
            match remove_typedef_qual typ with
            | T_c_bitfield(btyp, _) -> remove_typedef_qual btyp
            | t -> t
          in
          compare_typ (remove_typedef_qual e.etyp) typ' = 0

        | C_init_list(l, _) ->
          match_init_list_with_typ l typ

        | _ -> assert false
      in
      let field =
        List.find (fun field ->
            match_init_with_typ init field.c_field_type
          ) fields
      in
      flatten_init base (Some init) offset field.c_field_type range flow

    | Some (C_init_expr e) when is_c_record_type e.etyp ->
      [(e,offset,typ)],[]

    | Some init ->
      panic_at range "aggregate: union init %a not supported" Pp.pp_c_init init

  let mk_typed_zero typ range =
    if is_c_int_type typ then
      mk_zero ~typ range
    else
    if is_c_float_type typ then
      mk_float 0. ~prec:(get_c_float_precision typ) range
    else
    if is_c_pointer_type typ then
      mk_c_null range
    else
      mk_zero ~typ range

  (** 𝕊⟦ type v = init; ⟧ *)
  let declare v init scope range man flow =
    man.exec (mk_add_var v range) flow >>% fun flow ->
    let initl,fill = flatten_init v init Z.zero v.vtyp range flow in

    (* Scalar variables can be handed directly by the underlying low-level domain *)
    if is_c_scalar_type v.vtyp then
      match initl with
      | [e,o,t] ->
        let stmt = mk_assign (mk_var v range) e range in
        man.exec stmt flow

      | [] when is_c_global_scope scope ->
        let stmt = mk_assign (mk_var v range) (mk_typed_zero v.vtyp range) range in
        man.exec stmt flow

      | _ ->
        Post.return flow
    else
      (* Create a sequence of assignments for initializers *)
      List.fold_left
        (fun acc (e,o,t) ->
           (* Execute the assignment on the range of the expression *)
           let erange = e.erange in
           (* *(( t* )( char* )(&v) + o)) = e; *)
           let stmt = mk_assign (mk_c_deref (mk_c_cast
                                               (mk_binop
                                                  (mk_c_cast (mk_c_address_of (mk_var v erange) erange) (pointer_type s8) erange)
                                                  O_plus
                                                  (mk_z o erange)
                                                  ~etyp:(pointer_type s8) erange
                                               )
                                               (pointer_type t) erange
                                            ) erange) e erange
           in
           acc >>% man.exec stmt
        ) (Post.return flow) initl
      >>% fun flow ->

      (* For uninitialized parts, do nothing if the variable is local *)
      if not (is_c_global_scope scope) then
        Post.return flow
      else
        (* Global variable => initialize with zero *)
        fill |> List.fold_left
          (fun acc (filler, n, o, t) ->
             (* Initialisation expression *)
             let init =
               match filler with
               | Some e -> e
               | None -> mk_typed_zero t range (* global + uninitialized *)
             in
             let base =
               (* base : (char * )&v + o *)
               add
                 (mk_c_cast (mk_c_address_of (mk_var v range) range) (pointer_type s8) range)
                 (mk_z o range)
                 ~typ:(pointer_type s8) range
             in
             let bytes = Z.(n * sizeof_type t flow) in
             (* Use memset if the size is greater than the threshold *)
             if Z.(bytes >= of_int !opt_init_memset_threshold) then
               let i = mk_zero range in
               let j = mk_z Z.(pred bytes) range in
               memset base (mk_zero range) i j range man flow
             else
               (* Otherwise, use assignments *)
               let rec aux i acc =
                 if Z.(i = n) then acc
                 else
                   let stmt =
                     (* *(( t* )base + i) = init; *)
                     mk_assign
                       (mk_c_deref
                          (add
                             (mk_c_cast base (pointer_type t) range)
                             (mk_z i range) range) range)
                       init range
                   in
                   acc >>% fun flow ->
                   man.exec stmt flow |>
                   aux (Z.succ i)
               in
               aux Z.zero acc
          ) (Post.return flow)

  let copy_record_field lval rval field range man flow =
    let lval' = mk_c_member_access lval field range in
    let rval' = mk_c_member_access rval field range in
    match field.c_field_type |> remove_typedef_qual with

    | T_c_array(t,C_array_length_cst n) ->
      (* Copying cell-by-cell may be expensive, so use memcpy instead *)
      memcpy lval' rval' zero (mk_z Z.(n * sizeof_type t flow - one) range) range man flow

    | T_c_array _ ->
      (* Flexible array members are not copied (CC99 6.7.2.1.22) *)
      Post.return flow

    | _ ->
      let stmt = mk_assign lval' rval' range in
      man.exec stmt flow


  (** 𝕊⟦ lval = rval; ⟧ when lval is a struct *)
  let assign_struct lval rval range man flow =
    let rval = remove_casts rval in

    let t1 = lval |> etyp |> remove_typedef_qual
    and t2 = rval |> etyp |> remove_typedef_qual in

    if compare_typ t1 t2 <> 0 then
      panic_at range "[%s] assignment of struct with uncompatible \
                      types: %a %a" name pp_typ t1 pp_typ t2
    else
      (* Get the fields to copy *)
      let fields = match t1 with
        | T_c_record{c_record_fields} -> c_record_fields
        | _ -> assert false
      in

      (* Now copy the fields *)
      fields |> List.fold_left (fun acc field ->
          acc >>% copy_record_field lval rval field range man
        ) (Post.return flow)

  let assign_union lval rval range man flow =
    let t1 = lval |> etyp |> remove_typedef_qual
    and t2 = rval |> etyp |> remove_typedef_qual in
    let fields = match t1 with
      | T_c_record{c_record_fields} -> c_record_fields
      | _ -> assert false
    in
    if compare_typ t1 t2 = 0 then
      (* To copy a union, we just copy the largest field *)
      let fieldopt, _ = List.fold_left (fun (accfield, accsize) field ->
          let size = sizeof_type field.c_field_type flow in 
          if Z.geq size accsize then
            (Some field, size)
          else (accfield, accsize)
        ) (None, Z.zero) fields
      in
      match fieldopt with
      | Some field ->
        copy_record_field lval rval field range man flow

      | None ->
        panic_at range "[%s] all fields have size 0" name
    else
      (* Search for a field that has the same type as [rval] *)
      let fieldopt = List.find_opt (fun field -> compare_typ (remove_typedef_qual field.c_field_type) t2 = 0) fields in
      match fieldopt with
      | Some field ->
        let lval' = mk_c_member_access lval field range in
        man.exec (mk_assign lval' rval range) flow

      | None ->
        panic_at range "[%s] %a doesn't contain a field of type %a" name pp_typ t1 pp_typ t2


  let assign_bitfield p i f e range man flow =
    let t0 = under_type p.etyp in
    let field = find_c_record_field i t0 in
    let cell_offset, bitfield_offset, t = locate_bitfield_cell field t0 flow in
    let cell = mk_c_deref (mk_c_cast (add (mk_c_cast p (pointer_type u8) range) (mk_int cell_offset range) ~typ:(pointer_type u8) range) (pointer_type t) range) range in
    (* Mask of the bitfield *)
    let width = match remove_typedef_qual field.c_field_type with T_c_bitfield(_, w) -> w | _ -> assert false in
    let mask = Z.pow (Z.of_int 2) width |> Z.pred in
    let field_mask = Z.shift_left mask bitfield_offset in
    (* Inverted mask of the bitfield to preserve the remaining part of the cell *)
    let size = 8 * (sizeof_type t flow |> Z.to_int) in
    let inverted_field_mask = Z.logand (Z.lognot field_mask) (Z.pow (Z.of_int 2) size |> Z.pred) in
    (** Mask the expression (cell & 00..0011..1100..00) | ((e & 11..11) << OFFSET) *)
    let e' =
      mk_binop
        (mk_binop cell O_bit_and (mk_z inverted_field_mask range) ~etyp:t range)
        O_bit_or
        (mk_binop (mk_binop e O_log_and (mk_z mask range) ~etyp:e.etyp range) O_bit_lshift (mk_int bitfield_offset range) ~etyp:e.etyp range)
        ~etyp:t range
    in
    man.exec (mk_assign cell e' range) flow

  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration(v, init, scope) ->
      declare v init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, e)
    | S_expression { ekind = E_c_assign (lval, e) } when is_c_struct_type lval.etyp ->
      assign_struct lval e stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, e)
    | S_expression { ekind = E_c_assign (lval, e) } when is_c_union_type lval.etyp ->
      assign_union lval e stmt.srange man flow |>
      OptionExt.return

    | S_assign({ekind = E_c_member_access(s, i, f)}, e)
    | S_expression { ekind = E_c_assign ({ekind = E_c_member_access(s, i, f)}, e) }
      when is_c_bitfield (find_c_record_field i s.etyp).c_field_type ->
      assign_bitfield (mk_c_address_of s s.erange) i f e stmt.srange man flow |>
      Option.some

    | S_assign({ekind = E_c_arrow_access(p, i, f)}, e)
    | S_expression { ekind = E_c_assign ({ekind = E_c_arrow_access(p, i, f)}, e) }
      when is_c_bitfield (find_c_record_field i @@ under_type p.etyp).c_field_type ->
      assign_bitfield p i f e stmt.srange man flow |>
      Option.some

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ======================== *)

  let array_subscript a i t range = mk_lowlevel_subscript_access a i t range

  (** 𝔼⟦ s.f ⟧ -> *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
  let member_access s i f range = mk_lowlevel_member_access s i range

  (** 𝔼⟦ p->f ⟧ -> *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
  let arrow_access p i f t range = mk_lowlevel_arrow_access p i t range
  
  (** 𝔼⟦ &(a[i]) ⟧ = a + i *)
  let address_of_array_subscript a i t range = mk_binop a O_plus i ~etyp:t range

  (** 𝔼⟦ &(p->f) ⟧ = ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
  let address_of_arrow_access p i f t range =
    let st = under_type p.etyp in
    let align = mk_int (align_byte st i) range in
    mk_c_cast
      (mk_binop
         (mk_c_cast p (pointer_type s8) range)
         O_plus
         align
         range
      )
      (pointer_type t)
      range

  let eval exp man flow =
    match ekind exp with
    | E_c_array_subscript(a, i) ->
      man.eval (array_subscript a i exp.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_member_access (s, i, f) ->
      man.eval (member_access s i f exp.etyp exp.erange flow) flow |>
      OptionExt.return

    | E_c_arrow_access(p, i, f) ->
      man.eval (arrow_access p i f exp.etyp exp.erange flow) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_array_subscript(a,i)}) ->
      man.eval (add a i ~typ:a.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_member_access(s,i,f)} as e) ->
      man.eval (mk_lowlevel_field_address (mk_c_address_of s exp.erange) i e.etyp exp.erange) flow |>
      OptionExt.return

    | E_c_address_of({ekind = E_c_arrow_access(p,i,f)} as e) ->
      man.eval (mk_lowlevel_field_address p i e.etyp exp.erange) flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
