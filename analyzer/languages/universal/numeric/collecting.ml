(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2025-... The MOPSA Project.                                *)
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

(* A collecting interpreter P(V->Z)

   Should it be parameterized in the datatype?
   Could laziness help somewhere?
*)

open Mopsa_utils
open MopsaLib
open Lang.Ast
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

  let init _ =
    (* FIXME: StateCollection.empty can be bot *)
    Bot_top.Nbt (StateCollection.singleton (VarMap.empty)),
    [Alarm.A_prototype_domain name; Alarm.A_no_guaranteed_termination_domain name]

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let bcast op = fun x y -> if op x y then Z.one else Z.zero

  let rec eval expr (a : varmap) : ValueCollection.t =
    match ekind expr with
    | E_constant (C_bool b) -> ValueCollection.singleton (if b then Z.one else Z.zero)

    | E_binop((O_div | O_mod) as op, e1, e2) ->
      let$ v1 = eval e1 a in
      let$ v2 = eval e2 a |> ValueCollection.remove Z.zero in
      let zop = match op with
        | O_div -> Z.div
        | O_mod -> Z.(mod)
        | _ -> assert false in
      ValueCollection.singleton (zop v1 v2)

    | E_binop(op, e1, e2) ->
      let$ v1 = eval e1 a in
      let$ v2 = eval e2 a in
      let zop =
        match op with
        | O_plus -> Z.add
        | O_minus -> Z.sub
        | O_mult -> Z.mul
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
    (* FIXME: S_forget <> S_remove? S_add? *)
    match skind stmt with
    | S_add { ekind = E_var (v, _) } when accept_type v.vtyp ->
      Some a

    | S_assign ({ ekind= E_var (var, mode) }, e) when accept_type var.vtyp ->
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
      (Bot_top.bot_top_absorb1 (fun a ->
           let r = StateCollection.filter (fun a ->
               let vs = eval e a in
               let keep = ValueCollection.exists (fun v -> Z.(v <> zero)) vs in
               keep) a in
           if StateCollection.is_empty r then BOT else Nbt r (* FIXME: Bot = StateCollection.empty, to change... *)
         ) a)

    | S_forget  { ekind = E_var (v, _) }
    | S_remove { ekind = E_var (v, _) } when accept_type v.vtyp ->
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
           let+ input_state = a in
           VarMap.remove v input_state |>
           StateCollection.singleton
         ) a)

    | S_rename ({ ekind = E_var (var1, _) }, { ekind = E_var (var2, _) }) when accept_type var1.vtyp ->
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
           let+ input_state = a in
           VarMap.(
             let v1 = find var1 input_state in
             remove var1 input_state |>
             add var2 v1 |>
             StateCollection.singleton
           )
         ) a)

    | S_project vars
      when List.for_all (function { ekind = E_var _ } -> true | _ -> false) vars ->
      let vars = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vars
      in
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
           let+ input_state = a in
           VarMap.filter (fun v _ -> List.mem v vars) input_state
           |> StateCollection.singleton
         ) a)

    | S_expand ({ekind = E_var (v, _)}, vl)
      when accept_type v.vtyp &&
           List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
           let+ input_state = a in
           let value = VarMap.find v input_state in
           List.fold_left (fun acc v' ->
               VarMap.add v' value acc
             ) input_state vl
           |> StateCollection.singleton
         ) a)

    | S_fold ({ekind = E_var (v, mode)}, vl)
      when accept_type v.vtyp &&
           List.for_all (function { ekind = E_var _ } -> true | _ -> false) vl ->
      let vl = List.map (function
          | { ekind = E_var (v, _) } -> v
          | _ -> assert false
        ) vl
      in
      OptionExt.return @@
      (Bot_top.bot_top_lift1 (fun a ->
           let+ input_state = a in
           let values, map = List.fold_left (fun (accv, accm) v' ->
               let accv' = ValueCollection.add (VarMap.find v' input_state) accv in
               let accm' = VarMap.remove v' accm in
               accv', accm'
             ) (ValueCollection.empty, input_state) vl in
           let values =
             if VarMap.mem v input_state then
               ValueCollection.add (VarMap.find v input_state) values
             else
               values in
           ValueCollection.fold (fun value res ->
               StateCollection.add
                 (VarMap.add v value map)
                 res
             ) values StateCollection.empty
         ) a)

    | _ -> None

  let itv_of_valuecollection (v : ValueCollection.t) : Common.I.t =
    let lo, hi = ValueCollection.min_elt v, ValueCollection.max_elt v in
    Common.I.of_z lo hi

  let eval_into_itv (e: expr) (a : t) : Common.I.t Bot.with_bot =
    match a with
    | Bot_top.BOT -> Bot.BOT
    | Bot_top.TOP -> Nb (Common.I.minf_inf)
    | Bot_top.Nbt map ->
      (* FIXME: more disjoint cases *)
      Nb (
        StateCollection.fold (fun map acc -> ValueCollection.union acc (eval e map)) map ValueCollection.empty
        |>
        itv_of_valuecollection)

  let ask : type r.
    ('a,r) query ->
    ('a,t) Sig.Abstraction.Simplified.simplified_man ->
    'a ctx -> t -> r option =
    fun query man ctx map ->
    match query with
    | Q_avalue(e,Common.V_int_interval)
      when accept_type e.etyp ->
      OptionExt.return @@ eval_into_itv e map

    | Q_avalue(e,Common.V_int_interval_fast)
      when accept_type e.etyp ->
      OptionExt.return @@ eval_into_itv e map

    | Q_avalue(e,Common.V_int_congr_interval)
      when accept_type e.etyp ->
      OptionExt.return @@ (eval_into_itv e map, Bot.Nb Common.C.minf_inf)

    | _ ->
      None

  let print_expr man ctx a printer exp =
    let vals = Bot_top.bot_top_lift1 (fun a ->
        StateCollection.fold (fun map acc ->
            let vs = eval exp map in
            vs :: acc
          ) a []
      ) a in
    unformat
      (fun fmt v -> Format.fprintf fmt "%a"
          (Bot_top.bot_top_fprint (Format.pp_print_list
                                     (ValueCollection.fprint SetExt.printer_default Z.pp_print)
                                  )) v)
      printer
      ~path:[ Key "collecting";
              match ekind exp with
              | E_var (v, _) -> Obj (Var v)
              | _ -> fkey "%a" pp_expr exp ]
      vals

end

let () =
  register_simplified_domain (module Domain)
