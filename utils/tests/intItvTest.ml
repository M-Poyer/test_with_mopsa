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

let sample_z ((lo,hi):t) =
  let open QCheck2.Gen in
  match lo, hi with
  | B.MINF, B.PINF -> map Z.of_int int
  | B.Finite l, B.PINF -> map (Z.add l) (map Z.of_int (pint ~origin:0))
  | B.MINF, B.Finite h -> map (Z.sub h) (map Z.of_int (pint ~origin:0))
  | B.Finite l, B.Finite h -> map (Z.add l) (map (fun z -> Z.rem z (Z.succ (Z.sub h l))) (map Z.of_int (pint ~origin:0)))
  | _ -> failwith "IntItvTest.sample: invalid interval"

let generator_itv =
  let open QCheck2.Gen in
  bool >>= fun b ->
  match b with
  | false -> oneofl interesting_intervals
  | true ->
    pair (int_range (-1000) 1000) (int_range (-1000) 1000) >>= fun (x,y) ->
    let l,h = min x y, max x y in
    return (ItvUtils.IntItv.of_int l h)

let generator_z =
  let open QCheck2.Gen in
  generator_itv >>= fun itv ->
  sample_z itv >>= fun z ->
  return (itv,z)

let filter_shift _ (_,h) =
  B.is_positive h && B.leq h (B.of_int 256)

let filter_shift2 _ (_,h) _ =
  B.is_positive h && B.leq h (B.of_int 256)

module TestFactory = Common.Make(struct
  type t = ItvUtils.IntItv.t
  type c = Z.t
  let pp_t = ItvUtils.IntItv.fprint
  let pp_c = Z.pp_print
  let name_prefix = ""
  let gen = generator_z
  let filter_shift = filter_shift
  let filter_shift2 = filter_shift2
  let contains = contains
  let is_valid _ = true
end)

(* operator tests *)
(* ************** *)

let wrap2 lo up a =
  wrap a (Z.of_int lo) (Z.of_int up)

let bwd_wrap2 lo up a r =
  bwd_wrap a ((Z.of_int lo),(Z.of_int up)) r

let main () =
  let tests = [
    TestFactory.test_unop "-" Z.neg neg;
    TestFactory.test_unop "abs" Z.abs abs ;
    TestFactory.test_unop "succ" Z.succ succ ;
    TestFactory.test_unop "pred" Z.pred pred ;
    TestFactory.test_unop "wrap [0,255]" (Common.wrap_scalar 0 255) (wrap2 0 255);
    TestFactory.test_unop "wrap [-128,127]" (Common.wrap_scalar (-128) 127) (wrap2 (-128) 127);
    TestFactory.test_binop "+" Z.add add;
    TestFactory.test_binop "-" Z.sub sub;
    TestFactory.test_binop "*" Z.mul mul;
    TestFactory.test_binop_bot "/" Z.div div;
    TestFactory.test_binop_bot "/↓" Z.ediv ediv;
    TestFactory.test_binop_bot "%" Z.rem rem;
    TestFactory.test_binop_bot "%↓" Z.erem erem;
    TestFactory.test_binop_bot_shift "<<" (fun a b -> Z.shift_left a (Z.to_int b)) shift_left;
    TestFactory.test_binop_bot_shift ">>" (fun a b -> Z.shift_right a (Z.to_int b)) shift_right;
    TestFactory.test_binop_bot_shift ">>>" (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))) shift_right_trunc;
    TestFactory.test_unop "~" Z.lognot bit_not;
    TestFactory.test_binop "&" Z.logand bit_and;
    TestFactory.test_binop "|" Z.logor bit_or;
    TestFactory.test_binop "^" Z.logxor bit_xor;
    TestFactory.test_unop "bool" (fun a -> if a = Z.zero then Z.zero else Z.one) log_cast;
    TestFactory.test_unop "!" (fun a -> if a <> Z.zero then Z.zero else Z.one) log_not;
    TestFactory.test_binop "||" (fun a b -> if a <> Z.zero || b <> Z.zero then Z.one else Z.zero) log_or;
    TestFactory.test_binop "&&" (fun a b -> if a <> Z.zero && b <> Z.zero then Z.one else Z.zero) log_and;
    TestFactory.test_binop "^^" (fun a b -> if (a <> Z.zero) <> (b <> Z.zero) then Z.one else Z.zero) log_xor;
    TestFactory.test_binop "log ==" (fun a b -> if a = b then Z.one else Z.zero) log_eq;
    TestFactory.test_binop "log !=" (fun a b -> if a <> b then Z.one else Z.zero) log_neq;
    TestFactory.test_binop "log <" (fun a b -> if a < b then Z.one else Z.zero) log_lt;
    TestFactory.test_binop "log >" (fun a b -> if a > b then Z.one else Z.zero) log_gt;
    TestFactory.test_binop "log <=" (fun a b -> if a <= b then Z.one else Z.zero) log_leq;
    TestFactory.test_binop "log >=" (fun a b -> if a >= b then Z.one else Z.zero) log_geq;
    TestFactory.test_filter "filter =" (=) filter_eq;
    TestFactory.test_filter "filter !=" (<>) filter_neq;
    TestFactory.test_filter "filter <=" (<=) filter_leq;
    TestFactory.test_filter "filter >=" (>=) filter_geq;
    TestFactory.test_filter "filter <" (<) filter_lt;
    TestFactory.test_filter "filter >" (>) filter_gt;
    TestFactory.test_bwd_unop "bwd -" Z.neg bwd_neg;
    TestFactory.test_bwd_unop "bwd abs" Z.abs bwd_abs;
    TestFactory.test_bwd_unop "bwd succ" Z.succ bwd_succ;
    TestFactory.test_bwd_unop "bwd pred" Z.pred bwd_pred;
    TestFactory.test_bwd_unop "wrap [0,255]" (Common.wrap_scalar 0 255) (bwd_wrap2 0 255);
    TestFactory.test_bwd_unop "wrap [-128,127]" (Common.wrap_scalar (-128) 127) (bwd_wrap2 (-128) 127);
    TestFactory.test_bwd_binop "bwd +" Z.add bwd_add;
    TestFactory.test_bwd_binop "bwd -" Z.sub bwd_sub;
    TestFactory.test_bwd_binop "bwd *" Z.mul bwd_mul;
    TestFactory.test_bwd_binop "bwd /" Z.div bwd_div;
    TestFactory.test_bwd_binop "/↓" Z.ediv bwd_ediv;
    TestFactory.test_bwd_binop "%" Z.rem bwd_rem;
    TestFactory.test_bwd_binop "%↓" Z.erem bwd_erem;
    TestFactory.test_bwd_unop "bwd ~" Z.lognot bwd_bit_not;
    TestFactory.test_bwd_binop "bwd ^" Z.logxor bwd_bit_xor;
    TestFactory.test_bwd_binop "bwd |" Z.logor bwd_bit_or;
    TestFactory.test_bwd_binop "bwd &" Z.logand bwd_bit_and;
    TestFactory.test_bwd_binop_shift "bwd <<" (fun a b -> Z.shift_left a (Z.to_int b)) bwd_shift_left;
    TestFactory.test_bwd_binop_shift "bwd >>" (fun a b -> Z.shift_right a (Z.to_int b)) bwd_shift_right;
    TestFactory.test_bwd_binop_shift "bwd >>>" (fun a b -> Z.div a (Z.shift_left Z.one (Z.to_int b))) bwd_shift_right_trunc;
    TestFactory.test_bwd_binop "bwd log ==" (fun a b -> if a = b then Z.one else Z.zero) bwd_log_eq;
    TestFactory.test_bwd_binop "bwd log !=" (fun a b -> if a <> b then Z.one else Z.zero) bwd_log_neq;
    TestFactory.test_bwd_binop "bwd log <" (fun a b -> if a < b then Z.one else Z.zero) bwd_log_lt;
    TestFactory.test_bwd_binop "bwd log >" (fun a b -> if a > b then Z.one else Z.zero) bwd_log_gt;
    TestFactory.test_bwd_binop "bwd log <=" (fun a b -> if a <= b then Z.one else Z.zero) bwd_log_leq;
    TestFactory.test_bwd_binop "bwd log >=" (fun a b -> if a >= b then Z.one else Z.zero) bwd_log_geq;
  ] in
  QCheck_base_runner.run_tests_main tests

let _ = main ()
