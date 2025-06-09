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
  FloatItvTest - Unit tests for float intervals.
 *)

open Mopsa_utils
open Bot
module F = ItvUtils.Float
module I = ItvUtils.FloatItv
module IN = ItvUtils.FloatItvNan
open QCheck2.Gen

let nb_random_bounds = 6
let nb_random_bounds_bwd = 2
let nb_samples = 10
let nb_samples_bwd = 2
(* increase for more coverage... *)


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
     List.filter (fun f -> I.contains f i)
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
  (* FIXME: do we want to sometimes generate some float outside itv? *)
  generator_i prec round >>= fun itv ->
  sample_f_i prec round itv >>= fun f ->
  return (itv, f)

let generator_f_in prec round =
  (* currently, we only want some f in itv, so if itv is bottom, we have to retry
     alternative would be to assume and not assert containment  in test factory *)
  fix (fun self () ->
      generator_fi prec round >>= fun itv ->
      if IN.is_bot itv then self ()
      else
        sample_f_in prec round itv >>= fun f ->
        return (itv, f)) ()


let main () =
  let lift_filter op = fun a b -> Nb (op a b) in
  let lift_bot2 op = fun prec round a b -> Nb (op prec round a b) in
  let lift_bot3 op = fun prec round a b r -> Nb (op prec round a b r) in
  (* FIXME:
     - some test are precision or rounding independent, remove them
     - add support for `REAL *)
  let tests_i = List.map (fun (precs, precf, preci) ->
      List.map (fun (rounds, roundf, roundi) ->
          let module TestFactory = Common.Make(struct
              type t = I.t
              type c = F.t
              let pp_t = I.fprint I.dfl_fmt
              let pp_c = F.fprint F.dfl_fmt
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
            TestFactory.test_unop "-" F.neg I.neg;
            TestFactory.test_unop "abs" F.abs I.abs ;

            TestFactory.test_filter "filter =" (=) (I.filter_eq preci);
            TestFactory.test_filter "filter !=" (<>) (I.filter_neq preci);
            TestFactory.test_filter "filter <=" (<=) (I.filter_leq preci);
            TestFactory.test_filter "filter >=" (>=) (I.filter_geq preci);
            TestFactory.test_filter "filter <" (<) (I.filter_lt preci);
            TestFactory.test_filter "filter >" (>) (I.filter_gt preci);
            TestFactory.test_bwd_unop "bwd -" F.neg I.bwd_neg;
            TestFactory.test_bwd_unop "bwd abs" F.abs I.bwd_abs;
          ]
        )
        ["near", `NEAR, `NEAR; "up", `UP, `UP; "down", `DOWN, `DOWN; "zero", `ZERO, `ZERO]
    )
      ["double", `DOUBLE, `DOUBLE; "single", `SINGLE, `DOUBLE;] in
  let tests_in = List.map (fun (precs, precf, preci) ->
      List.map (fun (rounds, roundf, roundi) ->
           let module TestFactory = Common.Make(struct
               type t = IN.t
               type c = F.t
               let pp_t = IN.fprint IN.dfl_fmt
               let pp_c = F.fprint F.dfl_fmt
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
                 IN.contains f i end) in
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
             TestFactory.test_unop "-" F.neg IN.neg;
             TestFactory.test_unop "abs" F.abs IN.abs ;

             TestFactory.test_filter "filter =" (=) (lift_filter @@ IN.filter_eq preci);
             TestFactory.test_filter "filter !=" (<>) (lift_filter @@ IN.filter_neq preci);
             TestFactory.test_filter "filter <=" (<=) (lift_filter @@ IN.filter_leq preci);
             TestFactory.test_filter "filter >=" (>=) (lift_filter @@ IN.filter_geq preci);
             TestFactory.test_filter "filter <" (<) (lift_filter @@ IN.filter_lt preci);
             TestFactory.test_filter "filter >" (>) (lift_filter @@ IN.filter_gt preci);
             TestFactory.test_bwd_unop "bwd -" F.neg (fun a r -> Nb (IN.bwd_neg a r));
             TestFactory.test_bwd_unop "bwd abs" F.abs (fun a r -> Nb (IN.bwd_abs a r));
           ]
         )
          ["near", `NEAR, `NEAR; "up", `UP, `UP; "down", `DOWN, `DOWN; "zero", `ZERO, `ZERO]
      )
        ["double", `DOUBLE, `DOUBLE; "single", `SINGLE, `DOUBLE;]
  in
  QCheck_base_runner.run_tests_main (tests_i (*@ tests_in*) |> List.flatten |> List.flatten)

let () = main ()

(* (\* random non-NaN, non-infinity floats *\) *)
(* (\* *********************************** *\) *)


(* let rec sample_double () = *)
(*   let i1 = Int64.shift_left (Int64.of_int (Random.bits ())) 60 in *)
(*   let i2 = Int64.logxor i1 (Int64.shift_left (Int64.of_int (Random.bits ())) 30) in *)
(*   let i3 = Int64.logxor i2 (Int64.of_int (Random.bits ())) in *)
(*   let f = F.Double.of_bits i3 in *)
(*   if F.is_finite f then f else sample_double () *)
(* (\* random finite double in [-∞,+∞] *\) *)

(* let sample_double_itv (i:I.t) = *)
(*   match F.is_finite i.I.lo, F.is_finite i.I.up with *)
(*   | false, false -> sample_double () *)
(*   | true, false -> i.I.lo +. abs_float (sample_double ()) *)
(*   | false, true -> i.I.up -. abs_float (sample_double ()) *)
(*   | true, true -> *)
(*      if i.I.lo == i.I.up then i.I.lo *)
(*      else i.I.lo +. mod_float (abs_float (sample_double ())) (i.I.up -.i.I.lo) *)
(* (\* random finite double in i *\) *)

(* let rec sample_single_itv (i:I.t) = *)
(*   let d = sample_double_itv i in *)
(*   let f = F.Single.of_double_near d in *)
(*   if I.contains f i then f else sample_single_itv i *)
(* (\* random finite single precision in i *\) *)

(* let sample_nb sampler succ pred (i:I.t) nb = *)
(*   if I.is_singleton i then [i.I.lo] *)
(*   else *)
(*     (\* random values *\) *)
(*     let rec doit acc n = *)
(*       if n >= nb then acc *)
(*       else doit ((sampler i)::acc)(n+1) *)
(*     in *)
(*     let r = doit [] 0 in *)
(*     (\* special values *\) *)
(*     let r = if i.I.lo <= 0. && i.I.up >= 0. then 0.::r else r in *)
(*     let r = i.I.lo::i.I.up::r in *)
(*     List.sort_uniq compare r *)
(* (\* nb random samples from the interval, plus useful values *\) *)

(* let sample_double_nb = sample_nb sample_double_itv F.Double.succ F.Double.pred *)
(* let sample_single_nb = sample_nb sample_single_itv F.Single.succ F.Single.pred *)

(* let ext_sample_double_itv (ei:FI.t) = *)
(*   match ei with *)
(*   | { itv = Nb i } -> *)
(*     begin match F.is_finite i.I.lo && not ei.minf, F.is_finite i.I.up && not ei.pinf with *)
(*       | false, false -> sample_double () *)
(*       | true, false -> i.I.lo +. abs_float (sample_double ()) *)
(*       | false, true -> i.I.up -. abs_float (sample_double ()) *)
(*       | true, true -> *)
(*         if i.I.lo == i.I.up then i.I.lo *)
(*         else i.I.lo +. mod_float (abs_float (sample_double ())) (i.I.up -.i.I.lo) *)
(*     end *)
(*   | _ -> if ei.nan then nan else if ei.pinf then infinity else if ei.minf then neg_infinity else assert false *)
(* (\* FIXME: randomize depending on flags *\) *)
(* (\* random finite double in i *\) *)

(* let rec ext_sample_single_itv (i:FI.t) = *)
(*   let d = ext_sample_double_itv i in *)
(*   let f = F.Single.of_double_near d in *)
(*   if FI.contains f i then f else ext_sample_single_itv i *)
(* (\* random finite single precision in i *\) *)


(* let ext_sample_nb sampler succ pred (i:FI.t) nb = *)
(*   if FI.is_singleton i then *)
(*     [ *)
(*       match i with *)
(*       | { itv = Nb i } -> i.lo *)
(*       | _ -> *)
(*         if i.nan then nan *)
(*         else if i.pinf then infinity *)
(*         else if i.minf then neg_infinity *)
(*         else assert false *)
(*     ] *)
(*   else *)
(*     (\* random values *\) *)
(*     let rec doit acc n = *)
(*       if n >= nb then acc *)
(*       else doit ((sampler i)::acc)(n+1) *)
(*     in *)
(*     let r = doit [] 0 in *)
(*     (\* special values *\) *)
(*     let r = if FI.contains_zero i then 0.::r else r in *)
(*     let r = match i.itv with *)
(*       | Nb i -> i.lo :: i.up :: r *)
(*       | _ -> r in *)
(*     let r = if i.nan then nan :: r else r in *)
(*     let r = if i.pinf then infinity :: r else r in *)
(*     let r = if i.minf then neg_infinity :: r else r in  *)
(*     List.sort_uniq compare r *)
(* (\* nb random samples from the interval, plus useful values *\) *)

(* let ext_sample_double_nb = ext_sample_nb ext_sample_double_itv F.Double.succ F.Double.pred *)
(* let ext_sample_single_nb = ext_sample_nb ext_sample_single_itv F.Single.succ F.Single.pred *)


(* (\* random intervals *\) *)
(* (\* **************** *\) *)


(* let intervals interesting sampler nb = *)
(*   let b = interesting@(List.map (~-.) interesting) in *)
(*   let b1 = b@(sampler I.minf_inf nb) in *)
(*   let b2 = b@(sampler I.minf_inf nb) in *)
(*   let r = *)
(*     List.fold_left *)
(*       (fun acc lo -> *)
(*         List.fold_left *)
(*           (fun acc up -> *)
(*             if lo <= up then (I.of_float lo up)::acc *)
(*             else acc *)
(*           ) *)
(*           acc b1 *)
(*       ) *)
(*       [] b2 *)
(*   in *)
(*   List.sort_uniq compare r *)

(* let intervals_single = intervals interesting_bounds_single sample_single_nb *)
(* let intervals_double = intervals interesting_bounds_double sample_double_nb *)

(* let ext_intervals interesting sampler nb = *)
(*   let b = interesting@(List.map (~-.) interesting) in *)
(*   let b1 = b@(sampler I.minf_inf nb) in *)
(*   let b2 = b@(sampler I.minf_inf nb) in *)
(*   let r = *)
(*     List.fold_left *)
(*       (fun acc lo -> *)
(*          List.fold_left *)
(*            (fun acc up -> *)
(*               let acc = FI.join (FI.cst lo) (FI.cst up) :: acc in *)
(*               let acc = *)
(*                 try *)
(*                   if lo <= up then (FI.of_float lo up)::acc *)
(*                   else acc *)
(*                 with Invalid_argument _ -> acc *)
(*               in acc *)
(*            ) *)
(*            acc b1 *)
(*       ) *)
(*       [] b2 *)
(*   in *)
(*   let r = r @ [FI.bot; FI.pinf; FI.minf; FI.nan; FI.infinities; FI.specials] in *)
(*   List.sort_uniq compare r *)

(* let ext_intervals_single = ext_intervals interesting_bounds_single sample_single_nb *)
(* let ext_intervals_double = ext_intervals interesting_bounds_double sample_double_nb *)

(* (\* test operators with directed rounding *\) *)
(* (\* ************************************* *\) *)


(* let test_bin_val name i1 i2 i3 r1 r2 r3 = *)
(*   if F.is_finite r3 && not (I.contains r3 i3) then *)
(*     Printf.printf *)
(*       "ERROR %s: %a, %a -> %a / %g, %g -> %g\n%!" *)
(*       name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i2 (I.print I.dfl_fmt) i3 r1 r2 r3 *)

(* let test_bin_bot_val name i1 i2 i3 r1 r2 r3 = *)
(*   match i3 with *)
(*   | BOT -> *)
(*      if F.is_finite r3 then *)
(*        Printf.printf *)
(*          "ERROR %s: %a, %a -> ⊥ / %g, %g -> %g\n%!" *)
(*          name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i2 r1 r2 r3 *)
(*   | Nb i3 -> *)
(*     if F.is_finite r3 && not (I.contains r3 i3) then *)
(*       Printf.printf *)
(*         "ERROR %s: %a, %a -> %a / %g, %g -> %g\n%!" *)
(*         name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i2 (I.print I.dfl_fmt) i3 r1 r2 r3 *)

(* let test_bin_unmerged_val name i1 i2 l r1 r2 r3 = *)
(*   if F.is_finite r3 && not (List.exists (I.contains r3) l) then *)
(*     Printf.printf *)
(*       "ERROR %s: %a, %a -> %a / %g, %g -> %g\n%!" *)
(*       name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i2 *)
(*       (ListExt.print ListExt.printer_list (I.print I.dfl_fmt)) l *)
(*       r1 r2 r3 *)

(* let test_bin intervals sampler name test iop rop = *)
(*   Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       List.iter *)
(*         (fun i2 -> *)
(*           let i3 = iop i1 i2 in *)
(*           List.iter *)
(*             (fun r1 -> *)
(*               List.iter *)
(*                 (fun r2 -> *)
(*                   test name i1 i2 i3 r1 r2 (rop r1 r2) *)
(*                 ) *)
(*              (sampler i2 nb_samples) *)
(*             ) *)
(*             (sampler i1 nb_samples) *)
(*         ) *)
(*         (intervals nb_random_bounds) *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let test_bin_single name test iop rop = test_bin intervals_single sample_single_nb name test iop rop *)
(* let test_bin_double name test iop rop = test_bin intervals_double sample_double_nb name test iop rop *)

(* let test_bin_ext_double name test iop rop = test_bin ext_intervals_double ext_sample_double_nb name test iop rop *)

(* (\* let test_bin_ext_double name test iop rop = test_bin ext_intervals_double sample_double_nb name test iop rop  *\) *)

(* let test_un_val name i1 i3 r1 r3 = *)
(*   if F.is_finite r3 && not (I.contains r3 i3) then *)
(*     Printf.printf *)
(*       "ERROR %s: %a -> %a / %g -> %g\n%!" *)
(*       name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i3 r1 r3 *)

(* let test_un_bot_val name i1 i3 r1 r3 = *)
(*   match i3 with *)
(*   | BOT -> *)
(*      if F.is_finite r3 then *)
(*        Printf.printf *)
(*          "ERROR %s: %a -> ⊥ / %g, -> %g\n%!" *)
(*          name (I.print I.dfl_fmt) i1 r1 r3 *)
(*   | Nb i3 -> *)
(*     if F.is_finite r3 && not (I.contains r3 i3) then *)
(*       Printf.printf *)
(*         "ERROR %s: %a -> %a / %g -> %g\n%!" *)
(*         name (I.print I.dfl_fmt) i1 (I.print I.dfl_fmt) i3 r1 r3 *)


(* let test_un intervals sampler name test iop rop = *)
(*   Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       let i3 = iop i1 in *)
(*       List.iter *)
(*         (fun r1 -> *)
(*           test name i1 i3 r1 (rop r1) *)
(*         ) *)
(*         (sampler i1 nb_samples) *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let test_un_single name test iop rop = test_un intervals_single sample_single_nb name test iop rop *)
(* let test_un_double name test iop rop = test_un intervals_double sample_double_nb name test iop rop *)



(* (\* test operators with inner /outter rouinding *\) *)
(* (\* ******************************************* *\) *)


(* let inout_bin intervals name up down inner outer = *)
(*     Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       List.iter *)
(*         (fun i2 -> *)
(*           let u,d = up i1 i2, down i1 i2 *)
(*           and i,o = inner i1 i2, outer i1 i2 in *)
(*           let meet, join = I.meet u d, I.join u d in *)
(*           if not (I.equal_bot meet i) then *)
(*             Printf.printf *)
(*               "ERROR %s: up = %a, down = %a, inner = %a\n%!" *)
(*               name (I.print I.dfl_fmt) u (I.print I.dfl_fmt) d (I.print_bot I.dfl_fmt) i; *)
(*           if not (I.equal join o) then *)
(*             Printf.printf *)
(*               "ERROR %s: up = %a, down = %a, outer = %a\n%!" *)
(*               name (I.print I.dfl_fmt) u (I.print I.dfl_fmt) d (I.print I.dfl_fmt) o *)
(*         ) *)
(*         (intervals nb_random_bounds) *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let inout_bin_double = inout_bin intervals_double *)
(* let inout_bin_single = inout_bin intervals_single *)

(* let inout_bin_bot intervals name up down inner outer = *)
(*     Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       List.iter *)
(*         (fun i2 -> *)
(*           let u,d = up i1 i2, down i1 i2 *)
(*           and i,o = inner i1 i2, outer i1 i2 in *)
(*           let meet, join = I.meet_bot u d, I.join_bot u d in *)
(*           if not (I.equal_bot meet i) then *)
(*             Printf.printf *)
(*               "ERROR %s: up = %a, down = %a, inner = %a\n%!" *)
(*               name (I.print_bot I.dfl_fmt) u (I.print_bot I.dfl_fmt) d (I.print_bot I.dfl_fmt) i; *)
(*           if not (I.equal_bot join o) then *)
(*             Printf.printf *)
(*               "ERROR %s: up = %a, down = %a, outer = %a\n%!" *)
(*               name (I.print_bot I.dfl_fmt) u (I.print_bot I.dfl_fmt) d (I.print_bot I.dfl_fmt) o *)
(*         ) *)
(*         (intervals nb_random_bounds) *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let inout_bin_bot_double name up down inner outer = inout_bin_bot intervals_double name up down inner outer *)
(* let inout_bin_bot_single name up down inner outer = inout_bin_bot intervals_single name up down inner outer *)


(* let inout_un intervals name up down inner outer = *)
(*     Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       let u,d = up i1, down i1 *)
(*       and i,o = inner i1, outer i1 in *)
(*       let meet, join = I.meet u d, I.join u d in *)
(*       if not (I.equal_bot meet i) then *)
(*         Printf.printf *)
(*           "ERROR %s: up = %a, down = %a, inner = %a\n%!" *)
(*           name (I.print I.dfl_fmt) u (I.print I.dfl_fmt) d (I.print_bot I.dfl_fmt) i; *)
(*       if not (I.equal join o) then *)
(*         Printf.printf *)
(*           "ERROR %s: up = %a, down = %a outer = %a\n%!" *)
(*           name (I.print I.dfl_fmt) u (I.print I.dfl_fmt) d (I.print I.dfl_fmt) o *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let inout_un_double = inout_un intervals_double *)
(* let inout_un_single = inout_un intervals_single *)

(* let inout_un_bot intervals name up down inner outer = *)
(*     Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       let u,d = up i1, down i1 *)
(*       and i,o = inner i1, outer i1 in *)
(*       let meet, join = I.meet_bot u d, I.join_bot u d in *)
(*       if not (I.equal_bot meet i) then *)
(*         Printf.printf *)
(*           "ERROR %s: up = %a, down = %a, inner = %a\n%!" *)
(*           name (I.print_bot I.dfl_fmt) u (I.print_bot I.dfl_fmt) d (I.print_bot I.dfl_fmt) i; *)
(*       if not (I.equal_bot join o) then *)
(*         Printf.printf *)
(*           "ERROR %s: up = %a, down = %a, outer = %a\n%!" *)
(*           name (I.print_bot I.dfl_fmt) u (I.print_bot I.dfl_fmt) d (I.print_bot I.dfl_fmt) o *)
(*     ) *)
(*     (intervals nb_random_bounds) *)

(* let inout_un_bot_double name up down inner outer = inout_un_bot intervals_double name up down inner outer *)
(* let inout_un_bot_single name up down inner outer = inout_un_bot intervals_single name up down inner outer *)



(* (\* test filters *\) *)
(* (\* ************ *\) *)


(* let filter printer contains opname a1 a2 a3 c1 c2 c3 = *)
(*   match a3 with *)
(*   | BOT -> *)
(*      (try *)
(*         if c3 then *)
(*           Printf.printf *)
(*             "ERROR %s: %a, %a = ⊥; %a, %a = true\n" *)
(*             opname *)
(*             printer a1 printer a2 *)
(*             (F.print F.dfl_fmt) c1 (F.print F.dfl_fmt) c2 *)
(*       with _ -> () *)
(*      ) *)
(*   | Nb (aa1,aa2) -> *)
(*      (try *)
(*         Printf.printf "DEBUG %s: %a, %a = %a, %a; %a, %a\n" *)
(*           opname *)
(*           printer a1 printer a2 *)
(*           printer aa1 printer aa2 *)
(*           (F.print F.dfl_fmt) c1 (F.print F.dfl_fmt) c2; *)
(*         if c3 && (not (contains c1 aa1) || not (contains c2 aa2)) then *)
(*           Printf.printf *)
(*             "ERROR %s: %a, %a = %a, %a; %a, %a = true\n" *)
(*             opname *)
(*             printer a1 printer a2 *)
(*             printer aa1 printer aa2 *)
(*             (F.print F.dfl_fmt) c1 (F.print F.dfl_fmt) c2 *)
(*       with _ -> () *)
(*      ) *)


(* (\* test backward operators *\) *)
(* (\* *********************** *\) *)


(* let test_bwd_un_val opname a1 r a2 c1 c2 = *)
(*   match a2 with *)
(*   | BOT -> *)
(*      (try *)
(*         if I.contains c2 r then *)
(*           Printf.printf *)
(*             "ERROR %s: %a = %a -> ⊥;%g = %g\n" *)
(*             opname *)
(*             (I.print I.dfl_fmt) a1 (I.print I.dfl_fmt) r *)
(*             c1 c2 *)
(*       with _ -> () *)
(*      ) *)
(*   | Nb aa1 -> *)
(*      (try *)
(*         if I.contains c2 r && not (I.contains c1 aa1) then *)
(*           Printf.printf *)
(*             "ERROR %s: %a = %a -> %a; %g = %g\n" *)
(*             opname *)
(*             (I.print I.dfl_fmt) a1 (I.print I.dfl_fmt) r (I.print I.dfl_fmt) aa1 *)
(*             c1 c2 *)
(*       with _ -> () *)
(*      ) *)
(* (\* test contructor for backward unary operator *\) *)


(* let test_bwd_un intervals sampler name test iop rop = *)
(*   Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       List.iter *)
(*         (fun i2 -> *)
(*           let i3 = iop i1 i2 in *)
(*           List.iter *)
(*             (fun r1 -> *)
(*               test name i1 i2 i3 r1 (rop r1) *)
(*             ) *)
(*             (sampler i1 nb_samples_bwd) *)
(*         ) *)
(*         (intervals nb_random_bounds_bwd) *)
(*     ) *)
(*     (intervals nb_random_bounds_bwd) *)

(* let test_bwd_un_single name test iop rop = test_bwd_un intervals_single sample_single_nb name test iop rop *)
(* let test_bwd_un_double name test iop rop = test_bwd_un intervals_double sample_double_nb name test iop rop *)


(* let test_bwd_bin_val opname a1 a2 r a3 c1 c2 c3 = *)
(*   match a3 with *)
(*   | BOT -> *)
(*      (try *)
(*         if I.contains c3 r then *)
(*           Printf.printf *)
(*             "ERROR %s: %a, %a = %a -> ⊥; %g, %g = %g\n" *)
(*             opname *)
(*             (I.print I.dfl_fmt) a1 (I.print I.dfl_fmt) a2 (I.print I.dfl_fmt) r *)
(*             c1 c2 c3 *)
(*       with _ -> () *)
(*      ) *)
(*   | Nb (aa1,aa2) -> *)
(*      (try *)
(*         if I.contains c3 r && (not (I.contains c1 aa1) || not (I.contains c2 aa2)) then *)
(*           Printf.printf *)
(*             "ERROR %s: %a, %a = %a -> %a, %a; %g, %g = %g\n" *)
(*             opname *)
(*             (I.print I.dfl_fmt) a1 (I.print I.dfl_fmt) a2 (I.print I.dfl_fmt) r *)
(*             (I.print I.dfl_fmt) aa1 (I.print I.dfl_fmt) aa2 *)
(*             c1 c2 c3 *)
(*       with _ -> () *)
(*      ) *)
(* (\* test contructor for backward binary operator *\) *)


(* let test_bwd_bin intervals sampler name test iop rop = *)
(*   Printf.printf "checking %s\n%!" name; *)
(*   List.iter *)
(*     (fun i1 -> *)
(*       List.iter *)
(*         (fun i2 -> *)
(*           List.iter *)
(*             (fun i3 -> *)
(*               let i4 = iop i1 i2 i3 in *)
(*               List.iter *)
(*                 (fun r1 -> *)
(*                   List.iter *)
(*                     (fun r2 -> *)
(*                       test name i1 i2 i3 i4 r1 r2 (rop r1 r2) *)
(*                     ) *)
(*                     (sampler i2 nb_samples_bwd) *)
(*                 ) *)
(*                 (sampler i1 nb_samples_bwd) *)
(*             ) *)
(*             (intervals nb_random_bounds_bwd) *)
(*         ) *)
(*         (intervals nb_random_bounds_bwd) *)
(*     ) *)
(*     (intervals nb_random_bounds_bwd) *)

(* let test_bwd_bin_single name test iop rop = test_bwd_bin intervals_single sample_single_nb name test iop rop *)
(* let test_bwd_bin_double name test iop rop = test_bwd_bin intervals_double sample_double_nb name test iop rop *)



(* (\* entry point *\) *)
(* (\* *********** *\) *)


(* let test () = *)
(*   Printf.printf "hello\n"; *)

(*   (\* unary *\) *)

(*   test_un_double "neg" test_un_val I.neg F.neg; *)
(*   test_un_double "abs" test_un_val I.abs F.abs; *)


(*   (\* binary *\) *)

(*   test_bin_double "fmod" test_bin_bot_val I.fmod F.fmod; *)

(*   test_bin_double "add double near" test_bin_val I.Double.add_near F.Double.add_near; *)
(*   test_bin_double "add double up"   test_bin_val I.Double.add_up F.Double.add_up; *)
(*   test_bin_double "add double down" test_bin_val I.Double.add_down F.Double.add_down; *)
(*   test_bin_double "add double zero" test_bin_val I.Double.add_zero F.Double.add_zero; *)
(*   inout_bin_double "add double inner/outer" I.Double.add_up I.Double.add_down I.Double.add_inner I.Double.add_outer; *)

(*   test_bin_double "sub double near" test_bin_val I.Double.sub_near F.Double.sub_near; *)
(*   test_bin_double "sub double up"   test_bin_val I.Double.sub_up F.Double.sub_up; *)
(*   test_bin_double "sub double down" test_bin_val I.Double.sub_down F.Double.sub_down; *)
(*   test_bin_double "sub double zero" test_bin_val I.Double.sub_zero F.Double.sub_zero; *)
(*   inout_bin_double "sub double inner/outer" I.Double.sub_up I.Double.sub_down I.Double.sub_inner I.Double.sub_outer; *)

(*   test_bin_double "mul double near" test_bin_val I.Double.mul_near F.Double.mul_near; *)
(*   test_bin_double "mul double up"   test_bin_val I.Double.mul_up F.Double.mul_up; *)
(*   test_bin_double "mul double down" test_bin_val I.Double.mul_down F.Double.mul_down; *)
(*   test_bin_double "mul double zero" test_bin_val I.Double.mul_zero F.Double.mul_zero; *)
(*   inout_bin_double "mul double inner/outer" I.Double.mul_up I.Double.mul_down I.Double.mul_inner I.Double.mul_outer; *)

(*   test_bin_double "div double near" test_bin_bot_val I.Double.div_near F.Double.div_near; *)
(*   test_bin_double "div double up"   test_bin_bot_val I.Double.div_up F.Double.div_up; *)
(*   test_bin_double "div double down" test_bin_bot_val I.Double.div_down F.Double.div_down; *)
(*   test_bin_double "div double zero" test_bin_bot_val I.Double.div_zero F.Double.div_zero; *)
(*   inout_bin_bot_double "div double inner/outer" I.Double.div_up I.Double.div_down I.Double.div_inner I.Double.div_outer; *)

(*   test_bin_double "div unmerged double near" test_bin_unmerged_val I.Double.div_unmerged_near F.Double.div_near; *)
(*   test_bin_double "div unmerged double up"   test_bin_unmerged_val I.Double.div_unmerged_up F.Double.div_up; *)
(*   test_bin_double "div unmerged double down" test_bin_unmerged_val I.Double.div_unmerged_down F.Double.div_down; *)
(*   test_bin_double "div unmerged double zero" test_bin_unmerged_val I.Double.div_unmerged_zero F.Double.div_zero; *)

(*   test_un_double "sqrt double near" test_un_bot_val I.Double.sqrt_near F.Double.sqrt_near; *)
(*   test_un_double "sqrt double up"   test_un_bot_val I.Double.sqrt_up F.Double.sqrt_up; *)
(*   test_un_double "sqrt double down" test_un_bot_val I.Double.sqrt_down F.Double.sqrt_down; *)
(*   test_un_double "sqrt double zero" test_un_bot_val I.Double.sqrt_zero F.Double.sqrt_zero; *)
(*   inout_un_bot_double "sqrt double inner/outer" I.Double.sqrt_up I.Double.sqrt_down I.Double.sqrt_inner I.Double.sqrt_outer; *)

(*   test_un_double "round int double near" test_un_val I.Double.round_int_near F.Double.round_int_near; *)
(*   test_un_double "round int double up"   test_un_val I.Double.round_int_up F.Double.round_int_up; *)
(*   test_un_double "round int double down" test_un_val I.Double.round_int_down F.Double.round_int_down; *)
(*   test_un_double "round int double zero" test_un_val I.Double.round_int_zero F.Double.round_int_zero; *)
(*   inout_un_double "round int double inner/outer" I.Double.round_int_up I.Double.round_int_down I.Double.round_int_inner I.Double.round_int_outer; *)


(*   test_bin_single "add single near" test_bin_val I.Single.add_near F.Single.add_near; *)
(*   test_bin_single "add single up"   test_bin_val I.Single.add_up F.Single.add_up; *)
(*   test_bin_single "add single down" test_bin_val I.Single.add_down F.Single.add_down; *)
(*   test_bin_single "add single zero" test_bin_val I.Single.add_zero F.Single.add_zero; *)
(*   inout_bin_single "add single inner/outer" I.Single.add_up I.Single.add_down I.Single.add_inner I.Single.add_outer; *)

(*   test_bin_single "sub single near" test_bin_val I.Single.sub_near F.Single.sub_near; *)
(*   test_bin_single "sub single up"   test_bin_val I.Single.sub_up F.Single.sub_up; *)
(*   test_bin_single "sub single down" test_bin_val I.Single.sub_down F.Single.sub_down; *)
(*   test_bin_single "sub single zero" test_bin_val I.Single.sub_zero F.Single.sub_zero; *)
(*   inout_bin_single "sub single inner/outer" I.Single.sub_up I.Single.sub_down I.Single.sub_inner I.Single.sub_outer; *)

(*   test_bin_single "mul single near" test_bin_val I.Single.mul_near F.Single.mul_near; *)
(*   test_bin_single "mul single up"   test_bin_val I.Single.mul_up F.Single.mul_up; *)
(*   test_bin_single "mul single down" test_bin_val I.Single.mul_down F.Single.mul_down; *)
(*   test_bin_single "mul single zero" test_bin_val I.Single.mul_zero F.Single.mul_zero; *)
(*   inout_bin_single "mul single inner/outer" I.Single.mul_up I.Single.mul_down I.Single.mul_inner I.Single.mul_outer; *)

(*   test_bin_single "div single near" test_bin_bot_val I.Single.div_near F.Single.div_near; *)
(*   test_bin_single "div single up"   test_bin_bot_val I.Single.div_up F.Single.div_up; *)
(*   test_bin_single "div single down" test_bin_bot_val I.Single.div_down F.Single.div_down; *)
(*   test_bin_single "div single zero" test_bin_bot_val I.Single.div_zero F.Single.div_zero; *)
(*   inout_bin_bot_single "div single inner/outer" I.Single.div_up I.Single.div_down I.Single.div_inner I.Single.div_outer; *)

(*   test_bin_single "div unmerged single near" test_bin_unmerged_val I.Single.div_unmerged_near F.Single.div_near; *)
(*   test_bin_single "div unmerged single up"   test_bin_unmerged_val I.Single.div_unmerged_up F.Single.div_up; *)
(*   test_bin_single "div unmerged single down" test_bin_unmerged_val I.Single.div_unmerged_down F.Single.div_down; *)
(*   test_bin_single "div unmerged single zero" test_bin_unmerged_val I.Single.div_unmerged_zero F.Single.div_zero; *)

(*   test_un_single "sqrt single near" test_un_bot_val I.Single.sqrt_near F.Single.sqrt_near; *)
(*   test_un_single "sqrt single up"   test_un_bot_val I.Single.sqrt_up F.Single.sqrt_up; *)
(*   test_un_single "sqrt single down" test_un_bot_val I.Single.sqrt_down F.Single.sqrt_down; *)
(*   test_un_single "sqrt single zero" test_un_bot_val I.Single.sqrt_zero F.Single.sqrt_zero; *)
(*   inout_un_bot_single "sqrt single inner/outer" I.Single.sqrt_up I.Single.sqrt_down I.Single.sqrt_inner I.Single.sqrt_outer; *)

(*   test_un_single "round int single near" test_un_val I.Single.round_int_near F.Single.round_int_near; *)
(*   test_un_single "round int single up"   test_un_val I.Single.round_int_up F.Single.round_int_up; *)
(*   test_un_single "round int single down" test_un_val I.Single.round_int_down F.Single.round_int_down; *)
(*   test_un_single "round int single zero" test_un_val I.Single.round_int_zero F.Single.round_int_zero; *)
(*   inout_un_single "round int single inner/outer" I.Single.round_int_up I.Single.round_int_down I.Single.round_int_inner I.Single.round_int_outer; *)


(*   (\* filters *\) *)

(*   test_bin_double "filter double =" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_eq  (F.eq); *)
(*   test_bin_double "filter double ≠" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_neq (F.neq); *)
(*   test_bin_ext_double "filter double ≠" (filter (FI.print FI.dfl_fmt) FI.contains) (fun x y -> Bot.Nb (FI.filter_neq `DOUBLE x y)) (F.neq); *)
(*   test_bin_double "filter double <" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_lt  (F.lt); *)
(*   test_bin_double "filter double >" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_gt  (F.gt); *)
(*   test_bin_double "filter double ≤" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_leq (F.leq); *)
(*   test_bin_double "filter double ≥" (filter (I.print I.dfl_fmt) I.contains) I.Double.filter_geq (F.geq); *)

(*   test_bin_single "filter single =" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_eq  (F.eq); *)
(*   test_bin_single "filter single ≠" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_neq (F.neq); *)
(*   test_bin_single "filter single <" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_lt  (F.lt); *)
(*   test_bin_single "filter single >" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_gt  (F.gt); *)
(*   test_bin_single "filter single ≤" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_leq (F.leq); *)
(*   test_bin_single "filter single ≥" (filter (I.print I.dfl_fmt) I.contains) I.Single.filter_geq (F.geq); *)


(*   (\* backwards *\) *)

(*   test_bwd_un_double  "backward neg" test_bwd_un_val I.bwd_neg F.neg; *)
(*   test_bwd_un_double  "backward abs" test_bwd_un_val I.bwd_abs F.abs; *)
(*   test_bwd_bin_double "backward fmod" test_bwd_bin_val I.bwd_fmod F.fmod; *)

(*   test_bwd_bin_double "backward add double near" test_bwd_bin_val I.Double.bwd_add_near F.Double.add_near; *)
(*   test_bwd_bin_double "backward add double up  " test_bwd_bin_val I.Double.bwd_add_up   F.Double.add_up  ; *)
(*   test_bwd_bin_double "backward add double down" test_bwd_bin_val I.Double.bwd_add_down F.Double.add_down; *)
(*   test_bwd_bin_double "backward add double zero" test_bwd_bin_val I.Double.bwd_add_zero F.Double.add_zero; *)

(*   test_bwd_bin_double "backward sub double near" test_bwd_bin_val I.Double.bwd_sub_near F.Double.sub_near; *)
(*   test_bwd_bin_double "backward sub double up  " test_bwd_bin_val I.Double.bwd_sub_up   F.Double.sub_up  ; *)
(*   test_bwd_bin_double "backward sub double down" test_bwd_bin_val I.Double.bwd_sub_down F.Double.sub_down; *)
(*   test_bwd_bin_double "backward sub double zero" test_bwd_bin_val I.Double.bwd_sub_zero F.Double.sub_zero; *)

(*   test_bwd_bin_double "backward mul double near" test_bwd_bin_val I.Double.bwd_mul_near F.Double.mul_near; *)
(*   test_bwd_bin_double "backward mul double up  " test_bwd_bin_val I.Double.bwd_mul_up   F.Double.mul_up  ; *)
(*   test_bwd_bin_double "backward mul double down" test_bwd_bin_val I.Double.bwd_mul_down F.Double.mul_down; *)
(*   test_bwd_bin_double "backward mul double zero" test_bwd_bin_val I.Double.bwd_mul_zero F.Double.mul_zero; *)

(*   test_bwd_bin_double "backward div double near" test_bwd_bin_val I.Double.bwd_div_near F.Double.div_near; *)
(*   test_bwd_bin_double "backward div double up  " test_bwd_bin_val I.Double.bwd_div_up   F.Double.div_up  ; *)
(*   test_bwd_bin_double "backward div double down" test_bwd_bin_val I.Double.bwd_div_down F.Double.div_down; *)
(*   test_bwd_bin_double "backward div double zero" test_bwd_bin_val I.Double.bwd_div_zero F.Double.div_zero; *)

(*   test_bwd_un_double "backward round int double near" test_bwd_un_val I.Double.bwd_round_int_near F.Double.round_int_near; *)
(*   test_bwd_un_double "backward round int double up"   test_bwd_un_val I.Double.bwd_round_int_up   F.Double.round_int_up; *)
(*   test_bwd_un_double "backward round int double down" test_bwd_un_val I.Double.bwd_round_int_down F.Double.round_int_down; *)
(*   test_bwd_un_double "backward round int double zero" test_bwd_un_val I.Double.bwd_round_int_zero F.Double.round_int_zero; *)

(*   test_bwd_un_double "backward square double near" test_bwd_un_val I.Double.bwd_square_near F.Double.square_near; *)
(*   test_bwd_un_double "backward square double up"   test_bwd_un_val I.Double.bwd_square_up   F.Double.square_up; *)
(*   test_bwd_un_double "backward square double down" test_bwd_un_val I.Double.bwd_square_down F.Double.square_down; *)
(*   test_bwd_un_double "backward square double zero" test_bwd_un_val I.Double.bwd_square_zero F.Double.square_zero; *)

(*   test_bwd_un_double "backward sqrt double near" test_bwd_un_val I.Double.bwd_sqrt_near F.Double.sqrt_near; *)
(*   test_bwd_un_double "backward sqrt double up"   test_bwd_un_val I.Double.bwd_sqrt_up   F.Double.sqrt_up; *)
(*   test_bwd_un_double "backward sqrt double down" test_bwd_un_val I.Double.bwd_sqrt_down F.Double.sqrt_down; *)
(*   test_bwd_un_double "backward sqrt double zero" test_bwd_un_val I.Double.bwd_sqrt_zero F.Double.sqrt_zero; *)

(*   test_bwd_bin_single "backward add single near" test_bwd_bin_val I.Single.bwd_add_near F.Single.add_near; *)
(*   test_bwd_bin_single "backward add single up  " test_bwd_bin_val I.Single.bwd_add_up   F.Single.add_up  ; *)
(*   test_bwd_bin_single "backward add single down" test_bwd_bin_val I.Single.bwd_add_down F.Single.add_down; *)
(*   test_bwd_bin_single "backward add single zero" test_bwd_bin_val I.Single.bwd_add_zero F.Single.add_zero; *)

(*   test_bwd_bin_single "backward sub single near" test_bwd_bin_val I.Single.bwd_sub_near F.Single.sub_near; *)
(*   test_bwd_bin_single "backward sub single up  " test_bwd_bin_val I.Single.bwd_sub_up   F.Single.sub_up  ; *)
(*   test_bwd_bin_single "backward sub single down" test_bwd_bin_val I.Single.bwd_sub_down F.Single.sub_down; *)
(*   test_bwd_bin_single "backward sub single zero" test_bwd_bin_val I.Single.bwd_sub_zero F.Single.sub_zero; *)

(*   test_bwd_bin_single "backward mul single near" test_bwd_bin_val I.Single.bwd_mul_near F.Single.mul_near; *)
(*   test_bwd_bin_single "backward mul single up  " test_bwd_bin_val I.Single.bwd_mul_up   F.Single.mul_up  ; *)
(*   test_bwd_bin_single "backward mul single down" test_bwd_bin_val I.Single.bwd_mul_down F.Single.mul_down; *)
(*   test_bwd_bin_single "backward mul single zero" test_bwd_bin_val I.Single.bwd_mul_zero F.Single.mul_zero; *)

(*   test_bwd_bin_single "backward div single near" test_bwd_bin_val I.Single.bwd_div_near F.Single.div_near; *)
(*   test_bwd_bin_single "backward div single up  " test_bwd_bin_val I.Single.bwd_div_up   F.Single.div_up  ; *)
(*   test_bwd_bin_single "backward div single down" test_bwd_bin_val I.Single.bwd_div_down F.Single.div_down; *)
(*   test_bwd_bin_single "backward div single zero" test_bwd_bin_val I.Single.bwd_div_zero F.Single.div_zero; *)

(*   test_bwd_un_single "backward round int single near" test_bwd_un_val I.Single.bwd_round_int_near F.Single.round_int_near; *)
(*   test_bwd_un_single "backward round int single up"   test_bwd_un_val I.Single.bwd_round_int_up   F.Single.round_int_up; *)
(*   test_bwd_un_single "backward round int single down" test_bwd_un_val I.Single.bwd_round_int_down F.Single.round_int_down; *)
(*   test_bwd_un_single "backward round int single zero" test_bwd_un_val I.Single.bwd_round_int_zero F.Single.round_int_zero; *)

(*   test_bwd_un_single "backward square single near" test_bwd_un_val I.Single.bwd_square_near F.Single.square_near; *)
(*   test_bwd_un_single "backward square single up"   test_bwd_un_val I.Single.bwd_square_up   F.Single.square_up; *)
(*   test_bwd_un_single "backward square single down" test_bwd_un_val I.Single.bwd_square_down F.Single.square_down; *)
(*   test_bwd_un_single "backward square single zero" test_bwd_un_val I.Single.bwd_square_zero F.Single.square_zero; *)

(*   test_bwd_un_single "backward sqrt single near" test_bwd_un_val I.Single.bwd_sqrt_near F.Single.sqrt_near; *)
(*   test_bwd_un_single "backward sqrt single up"   test_bwd_un_val I.Single.bwd_sqrt_up   F.Single.sqrt_up; *)
(*   test_bwd_un_single "backward sqrt single down" test_bwd_un_val I.Single.bwd_sqrt_down F.Single.sqrt_down; *)
(*   test_bwd_un_single "backward sqrt single zero" test_bwd_un_val I.Single.bwd_sqrt_zero F.Single.sqrt_zero; *)

(*   () *)

(* let () = test () *)
