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

(** Bindings between Mopsa variables and Apron variables *)

open MopsaLib

module Equiv = Equiv.Make(
  struct
    type t = var
    let compare = compare_var
    let print fmt v = Format.fprintf fmt "%a" pp_var v
  end
  )(Apron.Var)

type t = Equiv.t

let empty : t = Equiv.empty

let mopsa_to_apron_var (v:var) (b:t) : Apron.Var.t * t =
  try Equiv.find_l v b, b
  with Not_found ->
    (* careful, this assumes vnames are unique, but compare_var also relies on the vtyp etc... This may create issues of vnames are not unique *)
    let vv = Apron.Var.of_string v.vname in
    vv, Equiv.add (v,vv) b

let mopsa_to_apron_vars (l:var list) (b:t) : Apron.Var.t list * t =
  List.fold_left (fun (accv,accb) v ->
      let vv, accb = mopsa_to_apron_var v accb in
      (vv::accv),accb
    ) ([],b) l

let apron_to_mopsa_var (v:Apron.Var.t) (b:t) : var =
  try Equiv.find_r v b
  with Not_found -> panic "Apron variable %s not bound" (Apron.Var.to_string v)

let apron_to_mopsa_vars (vs:Apron.Var.t list) (b:t) : var list =
  List.map (fun v -> apron_to_mopsa_var v b) vs

let concat b1 b2 =
  Equiv.concat b1 b2

(* DO NOT remove_apron_vars, the comparison on Apron Variables is much more sensitive than the one on Mopsa variables. The idea is that in Mopsa we can create range_attr_var with the same variable/range/attr multiple times, while Apron they will have a UID *)
let remove_var v b =
  Equiv.remove_l v b

let remove_vars vl b =
  List.fold_left (fun acc v -> remove_var v acc) b vl
