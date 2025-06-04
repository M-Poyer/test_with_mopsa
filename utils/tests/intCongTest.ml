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

(**
  CongItvTest - Unit tests for integer congruence domain.
 *)


open Mopsa_utils
open Bot
open CongUtils.IntCong


let mk_big () =
  Z.add (Z.shift_left (Z.of_int (Random.bits ())) 60)
        (Z.add (Z.shift_left (Z.of_int (Random.bits ())) 30)
               (Z.of_int (Random.bits ())))

let mk_big_z =
  let open QCheck2.Gen in
  triple (pint ~origin:0) (pint ~origin:0) (pint ~origin:0) >>= fun (x,y,z) ->
  return (
    Z.add (Z.shift_left (Z.of_int x) 60)
          (Z.add (Z.shift_left (Z.of_int y) 30)
                 (Z.of_int z)))

let mk_cong =
  let open QCheck2.Gen in
  pair mk_big_z mk_big_z >>= fun (a,b) ->
  (* check the invariant, i.e., a = 0 || a > b >= 0 *)
  if Z.zero = a then return (a,b) else
  if Z.gt a b then return (a,b) else
  return (a,Z.pred a) (* a-1 >= 0 as a > 0 *)

let mk_cst =
  let open QCheck2.Gen in
  pair bool mk_big_z >>= fun (b,x) ->
  match b with
  | true -> return (cst x)
  | false -> return (cst (Z.neg x))

let congs =
  [zero; one; mone; minf_inf;
   cst_int 2; cst_int 18; cst_int (-25);
   of_int 2 0; of_int 2 1;
   of_int 3 0; of_int 3 1; of_int 3 2;
   of_int 4 0; of_int 4 1; of_int 4 2; of_int 4 3;
   of_int 5 0; of_int 5 1; of_int 5 2;
   of_int 6 0; of_int 6 1; of_int 6 3; of_int 6 4;
   of_int 12 0; of_int 12 1; of_int 12 3; of_int 12 4; of_int 12 6;
   of_int 13 0; of_int 13 7;
   of_int 25 0; of_int 28 12; of_int 29993 23498;
  ]

let sample_cong ((a,b):t) =
  let open QCheck2.Gen in
  if a = Z.zero then (assert (contains b (a,b)); return b)
  else
    bool >>= fun p ->
    match p with
    | true ->
      let candidates = [b; Z.add b a; Z.sub b a;
        Z.add b (Z.mul a (Z.of_int 2));
        Z.add b (Z.mul a (Z.of_int 3));
        Z.add b (Z.mul a (Z.of_int 4));
        Z.sub b (Z.mul a (Z.of_int 2));
        Z.sub b (Z.mul a (Z.of_int 3));
        Z.sub b (Z.mul a (Z.of_int 4));
      ] in
      List.iter (fun x -> assert (contains x (a,b))) candidates;
      oneofl candidates
    | false ->
      pair bool mk_big_z >>= fun (is_sum,x) ->
      let z = ((if is_sum then Z.add else Z.sub) b (Z.mul a x)) in
      (if not (contains z (a,b)) then
        let s = Format.asprintf "%a %a %a" Z.pp_print z Z.pp_print a Z.pp_print b in
        raise (Failure s));
      return z

let generator_cong =
  let open QCheck2.Gen in
  int_bound 2 >>= fun n ->
  match n with
  | 0 -> oneofl congs
  | 1 -> mk_cong
  | _ -> mk_cst

let generator_z =
  let open QCheck2.Gen in
  generator_cong >>= fun cong ->
  sample_cong cong >>= fun z ->
  return (cong,z)

module TestFactory = Common.Make(struct
  type t = CongUtils.IntCong.t
  type c = Z.t
  let pp_t = CongUtils.IntCong.fprint
  let pp_c = Z.pp_print
  let name_prefix = ""
  let gen = generator_z
  let filter_shift = fun _ _ -> true
  let filter_shift2 = fun _ _ _ -> true
  let contains = CongUtils.IntCong.contains
end)

let wrap2 lo up a =
  wrap a (Z.of_int lo) (Z.of_int up)

let bwd_wrap2 lo up a r =
  bwd_wrap a ((Z.of_int lo),(Z.of_int up)) r

let test () =
  let tests = [
    TestFactory.test_unop "neg" Z.neg neg;
    TestFactory.test_unop "abs" Z.abs abs;
    TestFactory.test_unop "succ" Z.succ succ;
    TestFactory.test_unop "pred" Z.pred pred;
    TestFactory.test_unop "wrap [0,255]" (Common.wrap_scalar 0 255) (wrap2 0 255);
    TestFactory.test_unop "wrap [-128,127]" (Common.wrap_scalar (-128) 127) (wrap2 (-128) 127);
    TestFactory.test_binop "join left" (fun a b -> a) join;
    TestFactory.test_binop "join right" (fun a b -> b) join;
    (*TestFactory.test_binop_bot "meet" (fun a b -> if a <> b then QCheck2.assume_fail (); a) meet;*)
    TestFactory.test_binop "+" Z.add add;
    TestFactory.test_binop "-" Z.sub sub;
    TestFactory.test_binop "*" Z.mul mul;
    TestFactory.test_binop_bot "/" Z.div div;
    TestFactory.test_binop_bot "%" Z.rem rem;
    TestFactory.test_binop_bot "<<" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail ();  Z.shift_left a (Z.to_int b)) shift_left;
    TestFactory.test_binop_bot ">>" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail (); Z.shift_right a (Z.to_int b)) shift_right;
    TestFactory.test_binop_bot ">>>" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail (); Z.shift_right_trunc a (Z.to_int b)) shift_right_trunc;
    TestFactory.test_unop "~" Z.lognot bit_not;
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
    TestFactory.test_bwd_unop "bwd neg" Z.neg bwd_neg;
    TestFactory.test_bwd_unop "bwd abs" Z.abs bwd_abs;
    TestFactory.test_bwd_unop "bwd succ" Z.succ bwd_succ;
    TestFactory.test_bwd_unop "bwd pred" Z.pred bwd_pred;
    TestFactory.test_bwd_unop "bwd_wrap [0,255]" (Common.wrap_scalar 0 255) (bwd_wrap2 0 255);
    TestFactory.test_bwd_unop "bwd_wrap [-128,127]" (Common.wrap_scalar (-128) 127) (bwd_wrap2 (-128) 127);
    TestFactory.test_bwd_binop "bwd +" Z.add bwd_add;
    TestFactory.test_bwd_binop "bwd -" Z.sub bwd_sub;
    TestFactory.test_bwd_binop "bwd *" Z.mul bwd_mul;
    TestFactory.test_bwd_unop "bwd ~" Z.lognot bwd_bit_not;
    TestFactory.test_bwd_binop "bwd %" Z.rem bwd_rem;
    TestFactory.test_bwd_binop_shift "bwd <<" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail (); Z.shift_left a (Z.to_int b)) bwd_shift_left;
    TestFactory.test_bwd_binop_shift "bwd >>" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail (); Z.shift_right a (Z.to_int b)) bwd_shift_right;
    TestFactory.test_bwd_binop_shift "bwd >>>" (fun a b -> if Z.sign b < 0 || b >= Z.of_int 256 then QCheck2.assume_fail (); Z.div a (Z.shift_left Z.one (Z.to_int b))) bwd_shift_right_trunc;
  ] in
  QCheck_base_runner.run_tests_main tests

let _ = test ()
