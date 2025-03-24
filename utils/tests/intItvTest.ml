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

(**
  IntItvTest - Unit tests for integer intervals.

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
open ItvUtils.IntItv

let test_count = 1000
(* number of tests to execute *)



let one_two = of_int 1 2
let zero_two = of_int 0 2
let zero_ten = of_int 0 10
let mtwo_two = of_int (-2) 2
let mten_ten = of_int (-10)10
let one_inf = B.Finite Z.one, B.PINF
let mone_inf = B.Finite Z.minus_one, B.PINF
let two_inf = B.Finite (Z.of_int 2), B.PINF
let mtwo_inf = B.Finite (Z.of_int (-2)), B.PINF
let ten_inf = B.Finite (Z.of_int 10), B.PINF
let mten_inf = B.Finite (Z.of_int (-10)), B.PINF
let minf_one = B.MINF, B.Finite Z.one
let minf_mone = B.MINF, B.Finite Z.minus_one
let minf_two = B.MINF, B.Finite (Z.of_int 2)
let minf_mtwo = B.MINF, B.Finite (Z.of_int (-2))
let minf_ten = B.MINF, B.Finite (Z.of_int 10)
let minf_mten = B.MINF, B.Finite (Z.of_int (-10))
let ht = of_int 100 1000
let mht = of_int (-1000) (-100)

let interesting_intervals =
  [zero;one;mone;
   zero_one;mone_zero;mone_one;
   one_two;zero_two;zero_ten;mtwo_two;mten_ten;
   zero_inf;minf_zero;minf_inf;
   one_inf;mone_inf;two_inf;mtwo_inf;ten_inf;mten_inf;
   minf_one;minf_mone;minf_two;minf_mtwo;minf_ten;minf_mten;
   unsigned8;unsigned16;unsigned32;
   signed8;signed16;signed32;
   ht;mht;
  ]
(* some intersting intervals to test *)


(* random sampling *)
(* *************** *)

let generator_z ((lo,hi):t) =
  let open QCheck.Gen in
  match lo, hi with
  | B.MINF, B.PINF -> map Z.of_int int
  | B.Finite l, B.PINF -> map (Z.add l) (map Z.of_int pint)
  | B.MINF, B.Finite h -> map (Z.sub h) (map Z.of_int pint)
  | B.Finite l, B.Finite h -> map (Z.add l) (map (fun z -> Z.rem z (Z.succ (Z.sub h l))) (map Z.of_int pint))
  | _ -> failwith "IntItvTest.sample: invalid interval"

let arbitrary_z a =
  let open QCheck.Iter in
  let print z =
    Format.asprintf "%a ⇝ %a" ItvUtils.IntItv.fprint a Z.pp_print z
  in
  QCheck.make (generator_z a) ~print

let generator_z2 a a' =
  let open QCheck.Gen in
  pair (generator_z a) (generator_z a')

let arbitrary_z2 a a' =
  let open QCheck.Iter in
  let print (z,z') =
    Format.asprintf "(%a,%a) ⇝ (%a,%a)" ItvUtils.IntItv.fprint a ItvUtils.IntItv.fprint a' Z.pp_print z Z.pp_print z'
  in
  QCheck.make (generator_z2 a a') ~print


(* operator tests *)
(* ************** *)

(* forward operators *)
let test_unop (name:string) (cop: Z.t -> Z.t) (aop: t -> t) =
  let name = Format.sprintf "unop (%s)" name in
  let property a =
    let a' = aop a in
    fun c ->
    let c' = cop c in
    contains c' a'
  in
  List.map (fun a ->
    QCheck.Test.make ~count:test_count ~name (arbitrary_z a)
    (property a)
  ) interesting_intervals

let test_binop_gen ~contains ~filter (name:string) (cop: Z.t -> Z.t -> Z.t) (aop: t -> t -> 'a) =
  let name = Format.sprintf "binop (%s)" name in
  let property a a' =
    let a'' = aop a a' in
    fun (c,c') ->
    try
      let c'' = cop c c' in
      contains c'' a''
    with Division_by_zero | Invalid_argument _ -> true
  in
  List.map2 (fun a a' -> a,a') interesting_intervals interesting_intervals |>
  List.filter (fun (a,a') -> filter a a') |>
  List.map (fun (a,a') ->
    QCheck.Test.make ~count:test_count ~name (arbitrary_z2 a a')
    (property a a')
  )

let contains_bot c a = match a with
  | Nb a -> contains c a
  | BOT -> false

let filter_shift _ (_,h) =
  B.is_positive h && B.leq h (B.of_int 256)

let test_binop = test_binop_gen ~contains ~filter:(fun _ _ -> true)
let test_binop_bot = test_binop_gen ~contains:contains_bot ~filter:(fun _ _ -> true)
let test_binop_shift = test_binop_gen ~contains ~filter:filter_shift
let test_binop_bot_shift = test_binop_gen ~contains:contains_bot ~filter:filter_shift

(* backward operators *)
let test_bwd_unop (name:string) (cop: Z.t -> Z.t) (aop: t -> t -> t_with_bot) =
  let property a r =
    let a' = aop a r in (* refine `a` *)
    fun c ->
    let c' = cop c in
    if contains c' r then contains_bot c a' else true
  in
  List.map2 (fun a r ->
    QCheck.Test.make ~count:test_count ~name (arbitrary_z a)
    (property a r)
  ) interesting_intervals interesting_intervals

let test_bwd_binop_gen ~filter (name:string) (cop: Z.t -> Z.t -> Z.t) (aop: t -> t -> t -> (t*t) with_bot) =
  let property a a' r =
    let a',a'' = aop a a' r |> Bot.bot_split in (* refine `a` and `a'` *)
    fun (c,c') ->
    try
      let c'' = cop c c' in
      if contains c'' r then contains_bot c a' else true
    with Division_by_zero | Invalid_argument _ -> true
  in
  ListExt.map3 (fun a a' r -> a,a',r) interesting_intervals interesting_intervals interesting_intervals |>
  List.filter (fun (a,a',r) -> filter a a' r) |>
  List.map (fun (a,a',r) ->
    QCheck.Test.make ~count:test_count ~name (arbitrary_z2 a a')
    (property a a' r)
  )

let filter_shift _ (_,h) _ =
  B.is_positive h && B.leq h (B.of_int 256)

let test_bwd_binop = test_bwd_binop_gen ~filter:(fun _ _ _ -> true)
let test_bwd_binop_shift = test_bwd_binop_gen ~filter:filter_shift

let main () =
  let tests = [
    test_unop "-" Z.neg neg;
    test_unop "abs" Z.abs abs ;
    test_unop "succ" Z.succ succ ;
    test_unop "pred" Z.pred pred ;
    test_binop "+" Z.add add;
    test_binop "-" Z.sub sub;
    test_binop "*" Z.mul mul;
    test_binop_bot "/" Z.div div;
    test_binop_bot "/↓" Z.ediv ediv;
    test_binop_bot "%" Z.rem rem;
    test_binop_bot "%↓" Z.erem erem;
    test_binop_bot_shift "<<" (fun a b -> Z.shift_left a (Z.to_int b)) shift_left;
    test_binop_bot_shift ">>" (fun a b -> Z.shift_right a (Z.to_int b)) shift_right;
    test_binop_bot_shift ">>>" (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))) shift_right_trunc;
    test_unop "~" Z.lognot bit_not;
    test_binop "&" Z.logand bit_and;
    test_binop "|" Z.logor bit_or;
    test_binop "^" Z.logxor bit_xor;
    test_unop "bool" (fun a -> if a = Z.zero then Z.zero else Z.one) log_cast;
    test_unop "!" (fun a -> if a <> Z.zero then Z.zero else Z.one) log_not;
    test_binop "||" (fun a b -> if a <> Z.zero || b <> Z.zero then Z.one else Z.zero) log_or;
    test_binop "&&" (fun a b -> if a <> Z.zero && b <> Z.zero then Z.one else Z.zero) log_and;
    test_bwd_unop "bwd -" Z.neg bwd_neg;
    test_bwd_unop "bwd abs" Z.abs bwd_abs;
    test_bwd_unop "bwd succ" Z.succ bwd_succ;
    test_bwd_unop "bwd pred" Z.pred bwd_pred;
    test_bwd_binop "bwd +" Z.add bwd_add;
    test_bwd_binop "bwd -" Z.sub bwd_sub;
    test_bwd_binop "bwd *" Z.mul bwd_mul;
    test_bwd_binop "bwd /" Z.div bwd_div;
    test_bwd_binop "/↓" Z.ediv bwd_ediv;
    test_bwd_binop "%" Z.rem bwd_rem;
    test_bwd_binop "%↓" Z.erem bwd_erem;
    test_bwd_unop "bwd ~" Z.lognot bwd_bit_not;
    test_bwd_binop "bwd ^" Z.logxor bwd_bit_xor;
    test_bwd_binop "bwd |" Z.logor bwd_bit_or;
    test_bwd_binop "bwd &" Z.logand bwd_bit_and;
    test_bwd_binop_shift "bwd <<" (fun a b -> Z.shift_left a (Z.to_int b)) bwd_shift_left;
    test_bwd_binop_shift "bwd >>" (fun a b -> Z.shift_right a (Z.to_int b)) bwd_shift_right;
    test_bwd_binop_shift "bwd >>>" (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))) bwd_shift_right_trunc;
    test_bwd_binop "bwd log ==" (fun a b -> if a = b then Z.one else Z.zero) bwd_log_eq;
    test_bwd_binop "bwd log !=" (fun a b -> if a <> b then Z.one else Z.zero) bwd_log_neq;
    test_bwd_binop "bwd log <" (fun a b -> if a < b then Z.one else Z.zero) bwd_log_lt;
    test_bwd_binop "bwd log >" (fun a b -> if a > b then Z.one else Z.zero) bwd_log_gt;
    test_bwd_binop "bwd log <=" (fun a b -> if a <= b then Z.one else Z.zero) bwd_log_leq;
    test_bwd_binop "bwd log >=" (fun a b -> if a >= b then Z.one else Z.zero) bwd_log_geq;
  ] |> List.flatten in
  QCheck_base_runner.run_tests_main tests

let _ = main ()
