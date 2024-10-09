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

(** Relational numeric abstract domain, based on APRON. *)
(** CLIP abstraction with the all-real semantics, as described in the
    PhD thesis of Matthieu Journault **)

(* FIXME: reduction with itvs? *)

open Mopsa
open Sig.Abstraction.Simplified
open Common
open Ast
open Apron_manager
open Apron_transformer
open Domain 
(* FIXME: these are shared options, to fix *)

let opt_show_relational_domain = ref false
let () =
  register_domain_option "universal.numeric.relational" {
    key = "-show-relational-def-domain";
    category = "Numeric";
    doc = " display the domain on which the relational abstract state is defined";
    spec = ArgExt.Set opt_show_relational_domain;
    default = "false";
  }

let opt_enforce_sign_constraints = ref false
let () =
  register_shared_option "universal.numeric.relational" {
    key = "-enforce-sign-constraints";
    category = "Numeric";
    doc = " enforce sign constraints of variables in the relational domain";
    spec = ArgExt.Set opt_enforce_sign_constraints;
    default = "false";
  }

(* FIXME: redeclaring the query extension with already existing types does not create any issue... until the runtime that is *)

module Make(ApronManager : APRONMANAGER) =
struct


  include ApronTransformer(ApronManager)

  let related_vars v (a, bnd, l) = related_vars v (a, bnd)

  type t =
    ApronManager.t Apron.Abstract1.t (** Abstract element *) *
    Binding.t (** Bindings between Mopsa and Apron variables *) *
    VarSet.t (** under-approximation of domain *)

  include GenDomainId(struct
      type nonrec t = t
      let name = ApronManager.name
    end)

  let numeric_name = ApronManager.numeric_name

  (** {2 Command-line options} *)
  (** ************************ *)
  let () =
    import_shared_option rounding_option_name name

  (** {2 Environment utility functions} *)
  (** ********************************* *)


  let upper_dom bnd =
    Binding.Equiv.fold (fun (a, _) acc -> VarSet.add a acc) bnd VarSet.empty

  let check (a, bnd, low) =
    if not (VarSet.subset low (upper_dom bnd)) then
      panic "heterogeneous relational domain: structural definition broken@\n%a \\not\\subseteq %a"
        (VarSet.fprint SetExt.printer_default pp_var) low
        (VarSet.fprint SetExt.printer_default pp_var) (upper_dom bnd)
    ;
    (a, bnd, low) 

  let add_missing_vars (a,bnd,l) lv =
    check (a, bnd, l);
    let env = Apron.Abstract1.env a in
    let lv = List.sort_uniq compare lv in
    let lv = List.filter (fun v -> not @@ VarSet.mem v l) lv in

    let int_vars, bnd =
      let lv' = List.filter (fun v -> vtyp v = T_int || vtyp v = T_bool) lv in
      Binding.mopsa_to_apron_vars lv' bnd
    in

    let float_vars, bnd =
      let lv' = List.filter (function { vtyp = T_float _} -> true | _ -> false) lv in
      Binding.mopsa_to_apron_vars lv' bnd
    in

    let int_vars_inside, int_vars_outside =
      List.partition (Apron.Environment.mem_var env) int_vars in

    let float_vars_inside, float_vars_outside =
      List.partition (Apron.Environment.mem_var env) float_vars in

    let env_union = Apron.Environment.add env
        (Array.of_list int_vars_outside)
        (Array.of_list float_vars_outside)
    in

    let env_restricted = Apron.Environment.remove env (Array.of_list (int_vars_inside @ float_vars_inside)) in

    let l' = VarSet.union l (VarSet.of_list lv) in

    let a = Apron.Abstract1.change_environment ApronManager.man a env_restricted false in

    check
      (Apron.Abstract1.change_environment ApronManager.man a env_union false,
       bnd,
       l')

  let on_env (env : VarSet.t) (a, bnd) =
    (* FIXME: do we need to keep the update binding? *)
    let int_vars, float_vars = VarSet.partition (fun v -> vtyp v = T_int || vtyp v = T_bool) env in
    let apron_int_vars, _ = Binding.mopsa_to_apron_vars (VarSet.elements int_vars) bnd in
    let apron_float_vars, _ = Binding.mopsa_to_apron_vars (VarSet.elements float_vars) bnd in
    let apron_env = Apron.Environment.make (Array.of_list apron_int_vars) (Array.of_list apron_float_vars) in
    (* FIXME: check if flag should be true or false (ie, new dimensions should be to top, not 0 *)
    Apron.Abstract1.change_environment ApronManager.man a apron_env false


  let print_state printer (abs, bnd, l) =
    if !opt_show_relational_domain then
      let dom = Binding.Equiv.fold (fun (a, _) acc -> a::acc) bnd [] in
      let print_dom d = List
          (List.map (fun v -> String (Format.asprintf "%a" pp_var v))
             (List.sort compare_var d),
           { sopen = "{"; ssep = ","; sclose = "}"; sbind = "" }) in
      pp_obj_map printer
        [
          (String "domain (lower)", print_dom (VarSet.elements l));
          (String "domain (upper \\ lower)", print_dom (VarSet.elements @@ VarSet.diff (VarSet.of_list dom) l));
          (String "relations", pbox (Apron_pp.pp_env ApronManager.man) (abs, bnd));
        ]
        ~path:[Key "numeric-relations"]
    else
      pprint printer
        (pbox (Apron_pp.pp_env ApronManager.man) (abs, bnd))
        ~path:[Key "numeric-relations"]

  let proj_incl (abs1, bnd1) (abs2, bnd2) =
    (* FIXME: this operator should be exact. I'm not sure the OCaml interface allows checking that. In that case, all integer operations should be translated into ones on arbitrary-precision decimals *)
    assert(VarSet.subset (upper_dom bnd1) (upper_dom bnd2));
    let abs2 = Apron.Abstract1.change_environment ApronManager.man abs2 (Apron.Abstract1.env abs1) false in
    let () = debug "proj_incl,@\nabs1 = %a@\nabs2(m) = %a"
        (format print_state) (abs1, bnd1, upper_dom bnd1) (* unused *)
        (format print_state) (abs2, bnd2, upper_dom bnd2) (* unused *) in
    Apron.Abstract1.is_leq ApronManager.man abs1 abs2

  (** {2 Lattice operators} *)
  (** ********************* *)

  let top = Apron.Abstract1.top ApronManager.man empty_env, Binding.empty, VarSet.empty

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env, Binding.empty, VarSet.empty

  let is_bottom (abs,_,_) =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let subset (abs1,bnd1,l1) (abs2,bnd2,l2) =
    let dom1 = upper_dom bnd1 in
    let dom2 = upper_dom bnd2 in
    VarSet.subset l2 l1 &&
    VarSet.subset dom1 dom2 &&
    proj_incl (abs1, bnd1) (abs2, bnd2)

  let rec strengthening (r, bnd) a1 a2 (constraints: Apron.Lincons1.t list) =
    match constraints with
    | [] -> r
    | c :: ct ->
      let a_c =
        let env = Apron.Lincons1.get_env c in
        let a = Apron.Lincons1.array_make env 1 in
        Apron.Lincons1.array_set a 0 c; a in
      let test = Apron.Abstract1.meet_lincons_array
          ApronManager.man
          r
          a_c
      in
      let r =
        (* TODO: proj_incl could be replaced by sat_lincons in apron,
           but it seems overapproximated for the octagon impleemntation,
           what about the newpolka one? *)
        let b1 = proj_incl a1 (test, bnd) in
        let b2 = proj_incl a2 (test, bnd) in
        let () = debug "strengthening, can we keep %a? %b %b" Apron.Lincons1.print c b1 b2 in
        if b1 && b2 then
          test
        else r
      in
      strengthening (r, bnd) a1 a2 ct

  (* FIXME: can we obtain conflicting bindings? *)
  let join ((abs1,bnd1,l1) as a1) ((abs2,bnd2,l2) as a2) =
    if is_bottom a1 then a2
    else if is_bottom a2 then a1
    else
      let () = debug "join@\n%a@\n%a@\n"
          (format print_state) a1
          (format print_state) a2 in 
      let u = upper_dom bnd1 in
      let v = upper_dom bnd2 in
      let c = VarSet.inter u v in
      let () = debug "c = %a" (VarSet.fprint SetExt.printer_default pp_var) c in 
      let uv = VarSet.union u v in
      let bnd = Binding.concat bnd1 bnd2 in
      let r_1 =
        let a1_c = on_env c (abs1, bnd) in
        let a2_c = on_env c (abs2, bnd) in
        Apron.Abstract1.join ApronManager.man a1_c a2_c
      in
      let r_2 = on_env (VarSet.diff u c) (abs1, bnd) in
      let r_3 = on_env (VarSet.diff v c) (abs2, bnd) in
      let () = debug "r1=%a@\nr2=%a@\nr3=%a@\n"
          (format print_state) (on_env uv (r_1, bnd), bnd, VarSet.empty)
          (format print_state) (on_env uv (r_2, bnd), bnd, VarSet.empty)
          (format print_state) (on_env uv (r_3, bnd), bnd, VarSet.empty) in
      let r =
        Apron.Abstract1.meet_array ApronManager.man
          (Array.map (fun a -> on_env uv (a, bnd)) [|r_1; r_2; r_3|]), bnd in
      let constraints =
        let to_list cons =
          let rec read n =
            if n < 0 then []
            else
              let x = Apron.Lincons1.array_get cons n in
              x :: (read (n-1)) in
          read (Apron.Lincons1.array_length cons - 1) in 
        let filter_constraints upper inter cons =
          to_list cons |>
          List.filter (fun c ->
              let fv =
                let line = Apron.Lincons1.get_linexpr1 c in
                let lin_i, lin_f = Apron_pp.linexpr_to_list_pair (Apron.Lincons1.get_env c) line in
                VarSet.of_list @@ Binding.apron_to_mopsa_vars (List.map snd lin_i @ List.map snd lin_f) bnd in
              (not @@ VarSet.is_empty @@ VarSet.inter fv upper) &&
              (not @@ VarSet.is_empty @@ VarSet.inter fv (VarSet.diff upper inter))
            )
        in
        filter_constraints u c (Apron.Abstract1.to_lincons_array ApronManager.man abs1) @
        filter_constraints v c (Apron.Abstract1.to_lincons_array ApronManager.man abs2)
      in
      let () = debug "starting strengthening, from:@\nr = %a@\na1 = %a@\na2 = %a@\n"
          (format print_state) (fst r, snd r, c)
          (format print_state) a1
          (format print_state) a2
      in
      (* assert(subset a1 (fst r, snd r, c)); *)
      (* assert(subset a2 (fst r, snd r, c)); *)
      let a = strengthening r (abs1, bnd) (abs2, bnd) (List.rev constraints) in
      let low = VarSet.inter l1 l2 in
      let () = debug "join result@\n%a" (format print_state) (a, bnd, low) in 
      check (a, bnd, low)

  let meet (abs1,bnd1,l1) (abs2,bnd2,l2) =
    let u1 = upper_dom bnd1 in
    let u2 = upper_dom bnd2 in
    if not @@ VarSet.subset (VarSet.union l1 l2) (VarSet.inter u1 u2) then
      let () = debug "bad case (%a \\cup %a \\not\\subseteq %a \cap %a), bottom"
          (VarSet.fprint SetExt.printer_default pp_var) l1
          (VarSet.fprint SetExt.printer_default pp_var) l2
          (VarSet.fprint SetExt.printer_default pp_var) u1
          (VarSet.fprint SetExt.printer_default pp_var) u2
      in
      bottom
    else
      let bnd = Binding.concat bnd1 bnd2 in
      let n1 = on_env (VarSet.inter u1 u2) (abs1, bnd) in
      let n2 = on_env (VarSet.inter u1 u2)
          (on_env (VarSet.union l1 l2) (abs2, bnd), bnd) in
      let () = debug "%a@\n%a" (format print_state) (n1, bnd, l1) (format print_state) (n2, bnd, l2) in
      check (Apron.Abstract1.meet ApronManager.man n1 n2, bnd, VarSet.union l1 l2)

  let widen bnd (abs1,bnd1,l1) (abs2,bnd2,l2) =
    (* FIXME: can we really represent the upper set of defined variables through u1? *)
    let u1 = upper_dom bnd1 in
    let u2 = upper_dom bnd2 in
    if VarSet.subset l1 l2 && VarSet.subset u2 u1 then
      Apron.Abstract1.widening ApronManager.man abs1 (on_env u1 (abs2, bnd2)), bnd1, l1
    else if VarSet.subset l2 l1 && VarSet.subset u2 u1 then
      let r, bnd, _ = join (abs1, bnd1, l1) (abs2, bnd2, l2) in
      r, bnd, l2
    else
      let () = warn "widening to top:@\n%a@\n%a" (format print_state) (abs1, bnd1, l1) (format print_state) (abs2, bnd2, l2) in
      top

  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog =
    Apron_transformer.apron_int_to_rationals := true;
    top

  let remove_var (v:var) (a,bnd,l) =
    let env = Apron.Abstract1.env a in
    if is_env_var v (a,bnd) then
      let vv,bnd = Binding.mopsa_to_apron_var v bnd in
      let env = Apron.Environment.remove env [|vv|] in
      let bnd = Binding.remove_apron_var vv bnd in
      let l = VarSet.remove v l in
      (Apron.Abstract1.change_environment ApronManager.man a env true, bnd, l)
    else
      (a,bnd,l)


  let forget_var v (a,bnd,l) =
    let v,bnd = Binding.mopsa_to_apron_var v bnd in
    Apron.Abstract1.forget_array ApronManager.man a [|v|] false, bnd, l

  let bound_var v (abs,bnd,_) =
    if is_env_var v (abs,bnd) then
      let vv,_ = Binding.mopsa_to_apron_var v bnd in
      Apron.Abstract1.bound_variable ApronManager.man abs vv |>
      Values.Intervals.Integer.Value.of_apron
    else
      (* FIXME: remove even *)
      let () = warn "bound_var %a NOT IN ENVIRONMENT" pp_var v in 
      Values.Intervals.Integer.Value.top

  let is_var_numeric_type v = is_numeric_type (vtyp v)


  let assume stmt ask (a, bnd, low) =
    if is_bottom (a, bnd, low) then
      Some bottom else
    match skind stmt with
    | S_assume e ->
      begin
        let e_vars = Visitor.expr_vars e in
        let low' = VarSet.union low (VarSet.of_list (Visitor.expr_vars e)) in
        let () = debug "%a" (format print_state) (a, bnd, low) in 
        check (a, bnd, low);
        check (a, bnd, low');
        (* let a, bnd, low' = add_missing_vars (a,bnd,low) e_vars in *)
        try
          let dnf, a, bnd, l = bexp_to_apron (fun exp -> ask (Common.mk_float_maybenan_query exp)) e (a,bnd) [] in
          let env = Apron.Abstract1.env a in
          let a' =
            Dnf.reduce_conjunction
              (fun conj ->
                 let tcons_list =
                   List.map
                     (fun (op,e1,typ1,e2,typ2) ->
                        let typ =
                          if is_float_type typ1 || is_float_type typ2 then Apron.Texpr1.Real else
                          if is_int_type typ1 && is_int_type typ2 then Apron.Texpr1.Int
                          else Exceptions.panic_at (srange stmt)
                              "Unsupported case (%a, %a) in stmt @[%a@]"
                              pp_typ typ1 pp_typ typ2 pp_stmt stmt
                        in
                        let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
                        let diff_texpr = Apron.Texpr1.of_expr env diff in
                        Apron.Tcons1.make diff_texpr op
                     ) conj
                 in
                 tcons_array_of_tcons_list env tcons_list |>
                 Apron.Abstract1.meet_tcons_array ApronManager.man a
              ) ~join:(Apron.Abstract1.join ApronManager.man) dnf |>
            remove_tmp l
          in
          Some (a', bnd, low')
        with ImpreciseExpression -> Some (a,bnd,low')
           | UnsupportedExpression ->
             let () = debug "unsupported" in None
      end
    | _ -> assert false


  let merge (pre,bnd,pre_l) ((a1,bnd1,l1),e1) ((a2,bnd2,l2),e2) =
    let () = Debug.debug ~channel:(name ^ ".merge") "effects1 = %a@\neffects2 = %a@\na1 = %a@\na2 = %a"
        pp_effect e1
        pp_effect e2
        (format print_state) (a1, bnd1,l1)
        (format print_state) (a2, bnd2,l2)
    in
    let bnd = Binding.concat bnd1 bnd2 in
    (* let l = VarSet.union l1 l2 in *)
    let x1,x2 =
      generic_merge
        ~add:(fun v itv x ->
            let () = debug "adding %a" pp_var v in 
            if is_numerical_var v then
              let range = tag_range (Location.R_fresh (-1)) "relational merge" in 
              add_missing_vars x [v] |>
              forget_var v |>
              assume (mk_assume (constraints_of_itv (mk_var v range) itv range) range) (fun _ -> assert false) |>
              (* FIXME: should this variable be removed from the lower approximation? *)
              OptionExt.none_to_exn
            else
              x
          )
        ~find:(fun v x -> bound_var v x)
        ~remove:(fun v x ->
            let () = debug "removing %a" pp_var v in 
            if is_numerical_var v then remove_var v x else x)
        ((a1,bnd,l1),e1) ((a2,bnd,l2),e2)
    in
    let () = Debug.debug ~channel:(name ^ ".merge") "after generic merge@\na1 = %a@\na2 = %a@\nnow meeting results"
        (format print_state) x1
        (format print_state) x2 
    in
    meet x1 x2

  (** Add the sign contraint (if existing) of a given variable into the relationnal domain *)
  let enforce_sign_constraint var ask ctx range =
    if not !opt_enforce_sign_constraints then Fun.id else
    let fake_range = mk_tagged_range (String_tag "var sign constraint") range in
    match Framework.Combiners.Value.Nonrel.find_var_bounds_ctx_opt var ctx with
    | Some (C_int_interval (Finite l, Finite u)) when Z.equal l u -> 
      let stmt = mk_assume (mk_binop ~etyp:T_int (mk_var var fake_range) O_eq (mk_constant ~etyp:T_int (C_int l) fake_range) fake_range) fake_range in
      OptionExt.bind (assume stmt ask)
    | Some (C_int_interval (Finite l, _)) when Z.leq Z.zero l ->
      let stmt = mk_assume (mk_binop ~etyp:T_int (mk_var var fake_range) O_ge (mk_constant ~etyp:T_int (C_int l) fake_range) fake_range) fake_range in
      OptionExt.bind (assume stmt ask)
    | Some (C_int_interval (_, Finite u)) when Z.geq Z.zero u ->
      let stmt = mk_assume (mk_binop ~etyp:T_int (mk_var var fake_range) O_le (mk_constant ~etyp:T_int (C_int u) fake_range) fake_range) fake_range in
      OptionExt.bind (assume stmt ask)
    | _ ->
      Fun.id


  let rec exec stmt man ctx (a,bnd,low) =
    check (a, bnd, low);
    match skind stmt with
    | S_add { ekind = E_var (var, _) } when is_var_numeric_type var ->
      add_missing_vars (a,bnd,low) [var] |> check |> OptionExt.return |>
      enforce_sign_constraint var man.ask ctx stmt.srange

    | S_remove { ekind = E_var (var, _) } when is_var_numeric_type var ->
      remove_var var (a,bnd,low) |> check |> 
      OptionExt.return

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) when is_var_numeric_type var1 && is_var_numeric_type var2 ->
      let a, bnd, low = remove_var var2 (a,bnd,low) in
      let v1, _ = Binding.mopsa_to_apron_var var1 bnd in
      let bnd' = Binding.remove_apron_var v1 bnd in
      let low = if VarSet.mem var1 low then
          VarSet.add var2 (VarSet.remove var1 low)
        else
          VarSet.remove var1 low in
      let v2, bnd' = Binding.mopsa_to_apron_var var2 bnd' in
      (Apron.Abstract1.rename_array ApronManager.man a [| v1  |] [| v2 |], bnd', low) |> check |>
      OptionExt.return

    | S_assign({ ekind = E_var (var, mode) }, e) when var_mode var mode = STRONG && is_var_numeric_type var ->
      let () = debug "%a ~> %a" pp_stmt stmt (format print_state) (a, bnd, low) in 
      let a, bnd, low' = add_missing_vars (a, bnd, low) [var] in
      (* let a, bnd, low' = add_missing_vars (a, bnd, low) (Visitor.expr_vars e) in *)
      (* (var :: Visitor.expr_vars e) in *)
      let low' = VarSet.union low' (VarSet.of_list (Visitor.expr_vars e)) in
      let () = debug "post add_vars:@\n%a" (format print_state) (a, bnd, low') in 
      let low' = VarSet.union low (VarSet.of_list (Visitor.expr_vars e)) in
      check (a, bnd, low');
      let v, bnd = Binding.mopsa_to_apron_var var bnd in
      begin try
          let e, a, bnd, l = exp_to_apron (fun exp -> man.ask (Common.mk_float_maybenan_query exp)) e (a,bnd) [] in
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv e in
          let a' = Apron.Abstract1.assign_texpr ApronManager.man a v texp None |>
                   remove_tmp l
          in
          check (a', bnd, low');
          Some (a', bnd, low')
        with
        | ImpreciseExpression ->
          exec (mk_forget_var var stmt.srange) man ctx (a,bnd,low')
          |> OptionExt.lift (fun (a, bnd, low) -> let low = VarSet.add var low in (a, bnd, low))
          |> enforce_sign_constraint var man.ask ctx stmt.srange
        | UnsupportedExpression ->
          None
      end

    | S_assign({ ekind = E_var (var, mode) } as lval, e) when var_mode var mode = WEAK && is_var_numeric_type var ->
      let lval' = { lval with ekind = E_var(var, Some STRONG) } in
      exec {stmt with skind = S_assign(lval', e)} man ctx (a,bnd,low) |>
      OptionExt.lift @@ fun (a',bnd',low') ->
      join (a,bnd,low) (a', bnd',low')

    | S_expand({ekind = E_var (v, _)}, vl)
      when is_var_numeric_type v && List.for_all (function { ekind = E_var (v, _) } -> is_var_numeric_type v | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      let low = if VarSet.mem v low then VarSet.union low (VarSet.of_list vl) else low in
      let v, bnd = Binding.mopsa_to_apron_var v bnd in
      let vl, bnd = Binding.mopsa_to_apron_vars vl bnd in
      let abs' = Apron.Abstract1.expand ApronManager.man a v (Array.of_list vl) in
      Some (abs', bnd, low)

    | S_fold({ekind = E_var (v, _)}, vl)
      when is_var_numeric_type v && List.for_all (function { ekind = E_var (v, _) } -> is_var_numeric_type v | _ -> false) vl
      ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      (* FIXME: what about heterogeneous fold? *)
      assert(VarSet.subset (VarSet.of_list vl) low);
      let a, bnd,low = add_missing_vars (a,bnd,low) [v] in 
      let v, bnd = Binding.mopsa_to_apron_var v bnd in
      let vl, bnd = Binding.mopsa_to_apron_vars vl bnd in
      let bnd = Binding.remove_apron_vars vl bnd in
      let abs' = try Apron.Abstract1.fold ApronManager.man a (Array.of_list (v::vl))
                 with Apron.Manager.Error exc ->
                   panic_at stmt.srange "Apron.Manager.Error(%a)" Apron.Manager.print_exclog exc
      in
      Some (abs', bnd,low)

    | S_assume(e) when is_numeric_type (etyp e) ->
      assume stmt man.ask (a, bnd, low)

    | S_forget { ekind = E_var (var, _) } when is_var_numeric_type var ->
      forget_var var (a,bnd,low) |>
      OptionExt.return

    | S_project vars
      when List.for_all (function { ekind = E_var (v, _) } -> is_var_numeric_type v | _ -> false) vars
      ->
      let vars = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vars
      in
      let env = Apron.Abstract1.env a in
      let vars, bnd = Binding.mopsa_to_apron_vars vars bnd in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let bnd = Binding.remove_apron_vars to_remove bnd in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Some (
        Apron.Abstract1.change_environment ApronManager.man a new_env true,
        bnd,
        low
      )

    | _ -> None

  let vars (abs,bnd,_) =
    fold_env (fun v acc ->
        let vv = Binding.apron_to_mopsa_var v bnd in
        vv :: acc
      ) (Apron.Abstract1.env abs) []


  let rec eval_interval man e (abs,bnd,low) =
    match ekind e with
    | E_var (v,_) ->
      (* we meet with the interval computed by non-relational domains so that the interval of `unsigned x` is positive *)
      let int_nonrel = man.ask (Q_avalue(e, Common.V_int_interval_fast)) in
      let int_rel = bound_var v (abs,bnd,low) in
      Some (ItvUtils.IntItv.meet_bot int_nonrel int_rel)
    | E_binop (O_mult, e1, e2) ->
      let int1 = eval_interval man e1 (abs,bnd,low) in
      let int2 = eval_interval man e2 (abs,bnd,low) in
      begin match int1, int2 with
      | Some int1, Some int2 ->
        Bot.bot_lift2 ItvUtils.IntItv.mul int1 int2 |>
        OptionExt.return
      | _ -> None
      end
    | E_binop (O_convex_join, e1, e2) ->
      let int1 = eval_interval man e1 (abs,bnd,low) in
      let int2 = eval_interval man e2 (abs,bnd,low) in
      begin match int1, int2 with
      | Some int1, Some int2 ->
        ItvUtils.IntItv.join_bot int1 int2 |>
        OptionExt.return
      | _ -> None
      end
    | _ ->
      try
        let abs, bnd, low = add_missing_vars (abs,bnd,low) (Visitor.expr_vars e) in
        let e, abs, bnd, _ = exp_to_apron (fun exp -> man.ask (Common.mk_float_maybenan_query exp)) e (abs,bnd) [] in
        let env = Apron.Abstract1.env abs in
        let e = Apron.Texpr1.of_expr env e in
        Apron.Abstract1.bound_texpr ApronManager.man abs e |>
        Values.Intervals.Integer.Value.of_apron |>
        OptionExt.return
      with ImpreciseExpression -> Some (Values.Intervals.Integer.Value.top)
         | UnsupportedExpression -> None


  let ask : type r. ('a,r) query -> ('a,t) simplified_man -> 'a ctx -> t -> r option =
    fun query man ctx (abs,bnd,l) ->
      match query with
      | Q_avalue(e, Common.V_int_interval) ->
        eval_interval man e (abs,bnd,l)

      | Q_related_vars v ->
        related_vars v (abs,bnd,l) |>
        OptionExt.return

      | Q_constant_vars ->
        constant_vars (abs,bnd) |>
        OptionExt.return

      | _ ->
        None


  let print_expr man ctx (abs, bnd, low) printer exp =
    let a = abs, bnd in
    if exists_expr
        (fun e -> not (is_numeric_type e.etyp))
        (fun s -> false)
        exp
    then ()
    else
      let vars = expr_vars exp |> VarSet.of_list in
      let vars' = VarSet.fold (fun v acc ->
          all_related_vars v a |>
          VarSet.of_list |>
          VarSet.union acc
        ) vars vars in
      match VarSet.elements vars' with
      | [] -> ()
      | l  ->
        match exec (mk_project_vars l exp.erange) man ctx (abs, bnd, low) with
        | None -> ()
        | Some a -> print_state printer (abs, bnd, low)

end
