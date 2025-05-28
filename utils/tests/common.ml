(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(*
  Unit tests for numeric value domains

  This module uses property-based testing for four kind of operators:
    - Given a concrete operator ⋄ : 𝕫 → 𝕫, a sound abstraction of ⋄ is any
      operator ⋄# : A → A such that γ(⋄# D#) ⊇ ⋄ γ(D#).
      Testing of this condition is done directly by checking that the
      elements of the right-hand side belong also to the left-hand side.

    - This can be generalized to binary operators ⋄ : 𝕫 → 𝕫 → 𝕫,
      by letting ⋄# : A → A → A be any operator satisfying:
      γ(D1# ⋄# D2#) ⊇ (D1#) ⋄ γ(D2#). Testing is done in the same way.

    - Given a concrete operator ⋄ : 𝕫 → 𝕫 and a sound abstraction of it, we
      can define a backward refinement ⋄<-# : A → A → A any operator
      such that γ(⋄<-#(D#,R#)) ⊇ { v ∈ γ(D#) | ⋄ v ∈ γ(R#) }. Testing this
      condition is done by applying ⋄ to γ(D#) and, if the result belongs
      to γ(R#), checking that they belong to γ(⋄<-#(D#,R#)).

    - Likewise for a binary operator ⋄ : 𝕫 → 𝕫 → 𝕫, we let
      ⋄<-# : A → A → A → (A * A) any operator such that
      γ(⋄<-#(D1#,D2#,R#))_1 ⊇ { v  ∈ γ(D1#) | ∃v' ∈ γ(D2#). v ⋄ v' ∈ γ(R#) } and
      γ(⋄<-#(D1#,D2#,R#))_2 ⊇ { v' ∈ γ(D2#) | ∃v  ∈ γ(D1#). v ⋄ v' ∈ γ(R#) }.
      Testing is done in a similar way (we draw from both abstract elements).

*)

open Mopsa_utils
open Bot

let test_count = 100000
(* number of tests to execute *)

let wrap_scalar lo up a =
  let lo,up = Z.of_int lo, Z.of_int up in
  Z.add lo (Z.erem (Z.sub a lo) (Z.succ (Z.sub up lo)))

module type DOMAIN = sig
  type t (* abstract type *)
  type c (* concrete type *)
  val name_prefix : string
  val gen: (t * c) QCheck2.Gen.t
  val print : (t * c) QCheck2.Print.t
  val filter_shift: t -> t -> bool
  val filter_shift2: t -> t -> t -> bool
  val contains: c -> t -> bool
end

module Make(D: DOMAIN) = struct
  let gen = D.gen
  let print = D.print

  let gen2 = QCheck2.Gen.pair gen gen
  let print2 = QCheck2.Print.pair print print

  let gen3 = QCheck2.Gen.triple gen gen gen
  let print3 = QCheck2.Print.triple print print print

  let test_unop (name:string) (cop: D.c -> D.c) (aop: D.t -> D.t) =
    let name = Format.sprintf "%sunop (%s)" D.name_prefix name in
    let property (a,c) =
      let a' = aop a in
      let c' = cop c in
      D.contains c' a'
    in
    QCheck2.Test.make ~count:test_count ~name ~print gen property

  let test_binop_gen ~(contains:D.c -> 'a -> bool) ~filter (name:string) (cop: D.c -> D.c -> D.c) (aop: D.t -> D.t -> 'a) =
    let name = Format.sprintf "%sbinop (%s)" D.name_prefix name in
    let property ((a, c),(a', c')) =
      QCheck2.assume (filter a a');
      let a'' = aop a a' in
      try
        let c'' = cop c c' in
        contains c'' a''
      with Division_by_zero | Invalid_argument _ -> true
    in
    QCheck2.Test.make ~count:test_count ~name ~print:print2 gen2 property

  let contains_bot c a = match a with
    | Nb a -> D.contains c a
    | BOT -> false

  let test_binop = test_binop_gen ~contains:D.contains ~filter:(fun _ _ -> true)
  let test_binop_bot = test_binop_gen ~contains:contains_bot ~filter:(fun _ _ -> true)
  let test_binop_shift = test_binop_gen ~contains:D.contains ~filter:D.filter_shift
  let test_binop_bot_shift = test_binop_gen ~contains:contains_bot ~filter:D.filter_shift

  (* backward operators *)
  let test_bwd_unop (name:string) (cop: D.c -> D.c) (aop: D.t -> D.t -> D.t with_bot) =
    let property ((a,c),(r,_)) =
      let a' = aop a r in (* refine `a` *)
      let c' = cop c in
      if D.contains c' r then contains_bot c a' else true
    in
    QCheck2.Test.make ~count:test_count ~name:(D.name_prefix ^ name) ~print:print2 gen2 property

  let test_bwd_binop_gen ~filter (name:string) (cop: D.c -> D.c -> D.c) (aop: D.t -> D.t -> D.t -> (D.t * D.t) with_bot) =
    let property ((a,c),(a',c'),(r,_)) =
      QCheck2.assume (filter a a' r);
      let a'',a''' = aop a a' r |> Bot.bot_split in (* refine `a` and `a'` *)
      try
        let c'' = cop c c' in
        if D.contains c'' r then contains_bot c a'' && contains_bot c' a''' else true
      with Division_by_zero | Invalid_argument _ -> true
    in
    QCheck2.Test.make ~count:test_count ~name:(D.name_prefix ^ name) ~print:print3 gen3 property

  let test_filter (name:string) (cop: D.c -> D.c -> bool) (aop: D.t -> D.t -> (D.t * D.t) with_bot) =
    let property ((a,c),(a',c')) =
      assert (D.contains c a);
      assert (D.contains c' a');
      let a'', a''' = aop a a' |> Bot.bot_split in (* refine `a` and `a'` *)
      try
        if cop c c' then contains_bot c a'' && contains_bot c' a''' else true
      with Division_by_zero | Invalid_argument _ -> true
    in
    QCheck2.Test.make ~count:test_count ~name:(D.name_prefix ^ name) ~print:print2 gen2 property

  let test_bwd_binop = test_bwd_binop_gen ~filter:(fun _ _ _ -> true)
  let test_bwd_binop_shift = test_bwd_binop_gen ~filter:D.filter_shift2
end
