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

open Mopsa_utils
open Core.Bot
open Containers
module F = ItvUtils.Float
module I = ItvUtils.FloatItv
module IN = ItvUtils.FloatItvNan
open QCheck2.Gen

let interesting_bounds_double =
  let some = [0.; 1.; infinity; nan; F.Double.min_denormal; F.Double.min_normal; F.Double.max_normal; F.Double.max_exact] in
  List.sort_uniq Stdlib.compare (some @ List.map (fun x -> -. x) some)

let interesting_bounds_single =
  let some = [0.; 1.; infinity; nan; F.Single.min_denormal; F.Single.min_normal; F.Single.max_normal; F.Single.max_exact] in
  List.sort_uniq Stdlib.compare (some @ List.map (fun x -> -. x) some)

let float_extended_range i : F.t QCheck2.Gen.t =
  fix (fun self () ->
      float >>= fun f ->
      if I.contains f i then return f
      else self ()
    ) ()

let sample_f_i prec round (i : I.t) : F.t QCheck2.Gen.t =
  (bool >>= fun b ->
   let interesting =
     List.filter (fun f -> I.contains f i && not @@ F.is_nan f && not @@ Float.is_infinite f)
       (match prec with
        | `SINGLE -> interesting_bounds_single
        | `DOUBLE -> interesting_bounds_double) in
   let cast_to_good_type f =
     (* cast to single precision if needed *)
     match prec, round with
     | `SINGLE, `NEAR -> F.Single.of_double_near f
     | `SINGLE, `ZERO -> F.Single.of_double_zero f
     | `SINGLE, `UP   -> F.Single.of_double_up   f
     | `SINGLE, `DOWN -> F.Single.of_double_down f
     | `DOUBLE, _ -> f in
   (if b && interesting <> [] then oneofl interesting else
     match i.lo = neg_infinity, i.up = infinity with
     | true, true -> QCheck2.Gen.float
     | false, true -> map (F.add prec round i.lo) pfloat
     (* FIXME: not sure about rounding here to ensure the value lies within the range. In the meantime, I transformed some assert in assume in common
     I had some case where the result was [8.xxx, +infinity] and the sample predecessor of lower bound... *)
     | true, false -> map (F.sub prec round i.up) pfloat
     | false, false ->
       if i.up -. i.lo > max_float then
         float_extended_range i
       else
         QCheck2.Gen.float_range i.lo i.up
   ) |> map cast_to_good_type
  )


let sample_f_in prec round (i : IN.t) : F.t QCheck2.Gen.t =
  bool >>= fun b ->
  let interesting = List.filter (fun f -> IN.contains f i)
      (match prec with
      | `SINGLE -> interesting_bounds_single
      | `DOUBLE -> interesting_bounds_double) in
  if b && interesting <> [] then oneofl interesting
  else
    let special_values =
        (
          (if i.pinf then [infinity] else []) @
          (if i.minf then [neg_infinity] else []) @
          (if i.nan then [nan] else [])
        ) in
    match i.itv with
    | Nb fi ->
      bool >>= fun b ->
      if b || special_values = [] then
        sample_f_i prec round fi
      else
        oneofl special_values
    | _ -> oneofl special_values

let generator_i prec round =
  sample_f_i prec round I.minf_inf >>= fun x ->
  sample_f_i prec round I.minf_inf >>= fun y ->
  let lo, hi = min x y, max x y in
  return (I.of_float lo hi)

let generator_fi prec round =
  fix (fun self () ->
      bool >>= fun minf ->
      bool >>= fun pinf ->
      bool >>= fun nan ->
      bool >>= fun isbot ->
      if isbot then
        let r : IN.t = { itv=BOT; nan; pinf; minf } in
        if IN.is_valid r then return r else self ()
      else
        generator_i prec round >>= fun itv ->
        let r : IN.t = { itv=Nb itv; nan; pinf; minf } in
        if I.is_bounded itv && IN.is_valid r then return r else self ()) ()

let generator_f_i prec round =
  (* generates a tuple (itv, f) such that f in itv and f is not special *)
  (* FIXME: do we want to sometimes generate some float outside itv? *)
  generator_i prec round >>= fun itv ->
  fix (fun self () ->
      sample_f_i prec round itv >>= fun f ->
      if F.is_nan f || F.is_infinite f then self ()
      else return (itv, f)
    ) ()

let generator_f_in prec round =
  (* currently, we only want some f in itv, so if itv is bottom, we have to retry
     alternative would be to assume and not assert containment  in test factory *)
  fix (fun self () ->
      generator_fi prec round >>= fun itv ->
      if IN.is_bot itv then self ()
      else
        sample_f_in prec round itv >>= fun f ->
        return (itv, f)) ()

let forall_precs_rounds (f: (string * F.prec * 'a) -> (string * F.round * 'b) -> 'c list) =
  List.map (fun precs ->
      List.map (fun rounds -> f precs rounds)
        ["near", `NEAR, `NEAR; "up", `UP, `UP; "down", `DOWN, `DOWN; "zero", `ZERO, `ZERO;
         "near-any", `NEAR, `ANY; "up-any", `UP, `ANY; "down-any", `DOWN, `ANY; "zero-any", `ZERO, `ANY]
    )
    ["double", `DOUBLE, `DOUBLE; "single", `SINGLE, `SINGLE;]
  |> List.flatten |> List.flatten

let lift_filter op = fun a b -> Nb (op a b)
let lift_bot2 op = fun prec round a b -> Nb (op prec round a b)
let lift_bot3 op = fun prec round a b r -> Nb (op prec round a b r)

let main () =
  (* FIXME:
     - some test are precision or rounding independent, remove them
     - add support for `REAL *)
  let tests_i =
    forall_precs_rounds (fun (precs, precf, preci) (rounds, roundf, roundi) ->
          let module TestFactory = Common.Make(struct
              type t = I.t
              type c = F.t
              let pp_t = I.fprint I.hex_fmt
              let pp_c = F.fprint F.hex_fmt
              let name_prefix = "FloatItv("^ precs ^ ", " ^ rounds ^ ") "
              let gen = generator_f_i precf roundf
              let filter_shift a b = assert false
              let filter_shift2 a b r = assert false
              let contains f i =
                let () = match precf, roundf with
                  (* check that single-precision float is actually a single-precision float *)
                  | `SINGLE, `NEAR -> assert(F.is_nan f || F.Single.of_double_near f = f)
                  | `SINGLE, `ZERO -> assert(F.is_nan f || F.Single.of_double_zero f = f)
                  | `SINGLE, `UP   -> assert(F.is_nan f || F.Single.of_double_up   f = f)
                  | `SINGLE, `DOWN -> assert(F.is_nan f || F.Single.of_double_down f = f)
                  | _ -> () in
                (* FloatInterval does not handle well NaNs, so we won't focus on those here *)
                F.is_nan f || I.contains f i
              let is_valid f = not (F.is_nan f) && not (Float.is_infinite f)
            end) in
          (List.map (fun (sop, fop, iop, bwd_iop) ->
               [TestFactory.test_binop sop fop iop;
                TestFactory.test_bwd_binop ("bwd " ^ sop) fop bwd_iop]
             )
              ["+", F.add precf roundf, I.add preci roundi, I.bwd_add preci roundi;
               "-", F.sub precf roundf, I.sub preci roundi, I.bwd_sub preci roundi;
               "*", F.mul precf roundf, I.mul preci roundi, I.bwd_mul preci roundi; ]
           |> List.flatten)
          @
          [
            TestFactory.test_binop_bot "/" (F.div precf roundf) (I.div preci roundi);
            TestFactory.test_bwd_binop "bwd /" (F.div precf roundf) (I.bwd_div preci roundi);
            TestFactory.test_binop_bot "%" F.fmod  I.fmod;
            TestFactory.test_bwd_binop "bwd %" F.fmod I.bwd_fmod;

            TestFactory.test_unop "-" F.neg I.neg;
            TestFactory.test_bwd_unop "bwd -" F.neg I.bwd_neg;
            TestFactory.test_unop "abs" F.abs I.abs ;
            TestFactory.test_bwd_unop "bwd abs" F.abs I.bwd_abs;
            TestFactory.test_unop_bot "sqrt" (F.sqrt precf roundf) (I.sqrt preci roundi);
            TestFactory.test_bwd_unop "bwd sqrt" (F.sqrt precf roundf) (I.bwd_sqrt preci roundi);
            TestFactory.test_unop "round_int" (F.round_int precf roundf) (I.round_int preci roundi);
            TestFactory.test_bwd_unop "bwd round_int" (F.round_int precf roundf) (I.bwd_round_int preci roundi);

            TestFactory.test_filter "filter =" (=) (I.filter_eq preci);
            TestFactory.test_filter "filter !=" (<>) (I.filter_neq preci);
            TestFactory.test_filter "filter <=" (<=) (I.filter_leq preci);
            TestFactory.test_filter "filter >=" (>=) (I.filter_geq preci);
            TestFactory.test_filter "filter <" (<) (I.filter_lt preci);
            TestFactory.test_filter "filter >" (>) (I.filter_gt preci);

            TestFactory.test_join "join" I.join;
            TestFactory.test_join "widen is a join" I.widen;
            TestFactory.test_meet "meet" I.meet;
          ]
          @
          (* for rounding operators, I think we want to generate with a (precision, rounding) from TestFactory, but test all (roundings,precisions) *)
          forall_precs_rounds (fun (precs', precf', preci') (rounds', roundf', roundi') ->
              if precf' = precf && roundf' = roundf then []
              else
                let round = TestFactory.test_unop ("round-" ^ precs' ^ "-" ^ rounds')     (F.round precf roundf) (I.round preci roundi) in
                let bwd_round = TestFactory.test_bwd_unop ("bwd round-" ^ precs' ^ "-" ^ rounds') (F.round precf roundf) (I.bwd_round preci roundi) in
                match precf with
                | `SINGLE -> round::bwd_round::[]
                | _ ->
                  (* round for double precision is the identity, no need to test it *)
                  bwd_round::[]
            )
      )
  in
  let tests_in =
    forall_precs_rounds (fun (precs, precf, preci) (rounds, roundf, roundi) ->
           let module TestFactory = Common.Make(struct
               type t = IN.t
               type c = F.t
               let pp_t = IN.fprint IN.hex_fmt
               let pp_c = F.fprint F.hex_fmt
               let name_prefix = "FloatItvNan("^ precs ^ ", " ^ rounds ^ ") "
               let gen = generator_f_in precf roundf
               let filter_shift a b = assert false
               let filter_shift2 a b r = assert false
               let contains f i =
                 let () = match precf, roundf with
                   (* check that single-precision float is actually a single-precision float *)
                   | `SINGLE, `NEAR -> assert(F.is_nan f || F.Single.of_double_near f = f)
                   | `SINGLE, `ZERO -> assert(F.is_nan f || F.Single.of_double_zero f = f)
                   | `SINGLE, `UP   -> assert(F.is_nan f || F.Single.of_double_up   f = f)
                   | `SINGLE, `DOWN -> assert(F.is_nan f || F.Single.of_double_down f = f)
                   | _ -> () in
                 IN.contains f i
               let is_valid _ = true
             end) in
           (List.map (fun (sop, fop, iop, bwd_iop) ->
                [TestFactory.test_binop sop fop iop;
                 TestFactory.test_bwd_binop ("bwd " ^ sop) fop bwd_iop]
              )
               ["+", F.add precf roundf, IN.add preci roundi, (lift_bot3 IN.bwd_add) preci roundi;
                "-", F.sub precf roundf, IN.sub preci roundi, (lift_bot3 IN.bwd_sub) preci roundi;
                "*", F.mul precf roundf, IN.mul preci roundi, (lift_bot3 IN.bwd_mul) preci roundi; ]
            |> List.flatten)
           @
           [
             TestFactory.test_binop_bot "/" (F.div precf roundf) ((lift_bot2 IN.div) preci roundi);
             TestFactory.test_bwd_binop "bwd /" (F.div precf roundf) ((lift_bot3 IN.bwd_div) preci roundi);
             TestFactory.test_binop "%" F.fmod  (IN.fmod preci roundi);
             TestFactory.test_bwd_binop "bwd %" F.fmod (lift_bot3 IN.bwd_fmod preci roundi);

             TestFactory.test_unop "-" F.neg IN.neg;
             TestFactory.test_bwd_unop "bwd -" F.neg (fun a r -> Nb (IN.bwd_neg a r));
             TestFactory.test_unop "abs" F.abs IN.abs;
             TestFactory.test_bwd_unop "bwd abs" F.abs (fun a r -> Nb (IN.bwd_abs a r));
             TestFactory.test_unop "sqrt" (F.sqrt precf roundf) (IN.sqrt preci roundi);
             TestFactory.test_bwd_unop "bwd sqrt" (F.sqrt precf roundf) ((lift_bot2 IN.bwd_sqrt) preci roundi);
             TestFactory.test_unop "round_int" (F.round_int precf roundf) (IN.round_int preci roundi);
             TestFactory.test_bwd_unop "bwd round_int" (F.round_int precf roundf) (fun a r -> Nb (IN.bwd_round_int preci roundi a r));

             TestFactory.test_filter "filter =" (=) (lift_filter @@ IN.filter_eq preci);
             TestFactory.test_filter "filter !=" (<>) (lift_filter @@ IN.filter_neq preci);
             TestFactory.test_filter "filter <=" (<=) (lift_filter @@ IN.filter_leq preci);
             TestFactory.test_filter "filter >=" (>=) (lift_filter @@ IN.filter_geq preci);
             TestFactory.test_filter "filter <" (<) (lift_filter @@ IN.filter_lt preci);
             TestFactory.test_filter "filter >" (>) (lift_filter @@ IN.filter_gt preci);

             TestFactory.test_filter_zero "filter != 0" (fun x -> x <> 0.) (IN.filter_nonzero preci);
             TestFactory.test_filter_zero_false "filter != 0" (fun x ->  x <> 0.) (IN.filter_nonzero_false preci);


             TestFactory.test_filter_false "filter(false) <=" (<=) (lift_filter @@ IN.filter_leq_false preci);
             TestFactory.test_filter_false "filter(false) <" (<) (lift_filter @@ IN.filter_lt_false preci);
             TestFactory.test_filter_false "filter(false) >=" (>=) (lift_filter @@ IN.filter_geq_false preci);
             TestFactory.test_filter_false "filter(false) >" (>) (lift_filter @@ IN.filter_gt_false preci);

             TestFactory.test_join "join" IN.join;
             TestFactory.test_join "widen is a join" IN.widen;
             TestFactory.test_meet "meet" (fun a b -> Nb (IN.meet a b));
           ]
           @
           (* for rounding operators, I think we want to generate with a (precision, rounding) from TestFactory, but test all (roundings,precisions) *)
           forall_precs_rounds (fun (precs', precf', preci') (rounds', roundf', roundi') ->
               if precf' = precf && roundf' = roundf then []
               else
                 let round = TestFactory.test_unop ("round-" ^ precs' ^ "-" ^ rounds')     (F.round precf' roundf') (IN.round preci' roundi') in
                 let bwd_round = TestFactory.test_bwd_unop ("bwd round-" ^ precs' ^ "-" ^ rounds') (F.round precf' roundf') ((lift_bot2 IN.bwd_round) preci' roundi') in
                 match precf with
                 | `SINGLE -> round::bwd_round::[]
                 | _ ->
                   (* round for double precision is the identity, no need to test it *)
                   bwd_round::[]
             )
      )
  in
  QCheck_base_runner.run_tests_main (tests_i @ tests_in)

let () = main ()
