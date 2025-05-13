(* A collecting interpreter P(V->Z \cup F)

   Should it be parameterized in the datatype?

   Top is going to be expensive...

   Could laziness help somewhere?
*)

open Core.All
open Mopsa_utils
open Ast
open Sig.Abstraction.Simplified

module Domain : Sig.Abstraction.Simplified.SIMPLIFIED =
struct

  type varmap = Z.t VarMap.t

  module ValueCollection = SetExt.Make(Z)
  let (let$) vcol binder =
    ValueCollection.fold (fun v -> ValueCollection.union (binder v)) vcol ValueCollection.empty

  module StateCollection = SetExt.Make(
    struct
      type t = Z.t VarMap.t
      let compare = VarMap.compare Z.compare
    end)
  let ( let+ ) (state : StateCollection.t) (binder : varmap -> StateCollection.t) : StateCollection.t =
    StateCollection.fold (fun input_state ->
        StateCollection.union (binder input_state)
      ) state StateCollection.empty

  type t = StateCollection.t Bot_top.with_bot_top


  include GenDomainId(struct
      type nonrec t = t
      let name = "universal.numeric.collecting"
    end)

  let print_state printer a =
    Print.pprint printer ~path:[Key "collecting"]
      (pbox
         (unformat
            (Bot_top.bot_top_fprint
               (StateCollection.fprint SetExt.printer_default
                  (VarMap.fprint {MapExt.printer_default with print_arrow=" = "; print_sep=", "}
                     pp_var
                     Z.pp_print
                  )
               )
            )
         )
         a
      )

  let bottom = Bot_top.BOT

  let top = Bot_top.TOP

  let is_bottom = function
    | Bot_top.BOT -> true
    | _ -> false

  let subset = Bot_top.bot_top_included StateCollection.subset

  let join = Bot_top.bot_top_neutral2 StateCollection.union

  let meet = Bot_top.bot_top_absorb2 (fun x y -> Nbt (StateCollection.inter x y))

  let widen _ = join

  let merge _ _ _ = failwith "ni"

  let init _ = Bot_top.Nbt (StateCollection.singleton (VarMap.empty))
  (* FIXME: StateCollection.empty can be bot *)

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let bcast op = fun x y -> if op x y then Z.one else Z.zero 

  let rec eval expr (a : varmap) : ValueCollection.t =
    match ekind expr with
    | E_constant (C_bool b) -> ValueCollection.singleton (if b then Z.one else Z.zero)

    | E_binop(op, e1, e2) ->
      (* FIXME: what to do for divisions by zero? *)
      let$ v1 = eval e1 a in
      let$ v2 = eval e2 a in
      let zop =
        match op with
        | O_plus -> Z.add
        | O_minus -> Z.sub
        | O_mult -> Z.mul
        | O_div -> Z.div
        | O_mod -> Z.rem
        | O_le -> bcast Z.leq
        | O_lt -> bcast Z.lt
        | O_gt -> bcast Z.gt
        | O_ge -> bcast Z.geq
        | O_ne -> bcast (fun x y -> not (Z.equal x y))
        | O_eq -> bcast Z.equal
        | _ -> assert false in
      ValueCollection.singleton (zop v1 v2)


    | E_unop(O_log_not, e) ->
      let$ v = eval e a in
      if Z.equal v Z.one then ValueCollection.singleton Z.zero
      else if Z.equal v Z.zero then ValueCollection.singleton Z.one
      else assert false

    | E_var (v, _) ->
      VarMap.find v a |> ValueCollection.singleton

    | E_constant (C_int z) -> ValueCollection.singleton z

    | E_constant (C_int_interval (ItvUtils.IntBound.Finite l, ItvUtils.IntBound.Finite r)) ->
      let rec process i acc =
        if Z.leq i r then
          process Z.(add i one) (ValueCollection.add i acc)
        else acc
      in
      process l ValueCollection.empty

    | _ -> failwith "todo"


  let exec stmt sman ctx (a : t) =
    match skind stmt with
    | S_add { ekind = E_var (v, _) } when accept_type v.vtyp ->
      (* ??? Don't do anything here? *)
      Some a

    | S_assign ({ ekind= E_var (var, mode) }, e) when accept_type var.vtyp ->
      let () = debug "%a %a" pp_stmt stmt (format print_state) a in
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
          let+ input_state = a in
          let vs = eval e input_state in
          let () = debug "|eval %a|=%d" pp_expr e (ValueCollection.cardinal vs) in
          ValueCollection.fold (fun v acc -> StateCollection.add (VarMap.add var v input_state) acc) vs StateCollection.empty)
          a
      )

    | S_assume e ->
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (StateCollection.filter (fun a ->
          let vs = eval e a in
          let () = debug "assume %a = %a" pp_expr e (ValueCollection.fprint SetExt.printer_default Z.pp_print) vs in
          ValueCollection.exists (fun v -> Z.(v <> zero)) vs)) a)

    (* to support eventually *)
    (* | S_remove { ekind = E_var (v, _) } when Value.accept_type v.vtyp *)
    (* | S_project vars *)
    (*   when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars -> *)
    (* | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) when Value.accept_type var1.vtyp -> *)
    (* | S_forget { ekind = E_var (var, _) } when Value.accept_type var.vtyp -> *)
    (* | S_expand ({ekind = E_var (v, _)}, vl) *)
    (*   when Value.accept_type v.vtyp && *)
    (*        List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl *)
    (*      | S_fold ({ekind = E_var (v, mode)}, vl) *)
    (*   when Value.accept_type v.vtyp && *)
    (*        List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl *)

    | _ -> None



  let ask _ _ _ _ = None

  let print_expr _ _ _ _ _ = ()

end

let () = register_simplified_domain (module Domain)
