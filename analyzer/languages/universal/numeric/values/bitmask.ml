(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2025 The MOPSA Project.                               *)
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

(** Abstraction of integers with bitmasks *)

open Mopsa
open Sig.Abstraction.Simplified_value
open Ast
open Bitfields
open Bot

module Value =
struct

  type t = IntBitfields.t_with_bot

  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.bitmask"
        let display = "bitmask"
    end
    )

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let bottom = BOT

  let top = Nb IntBitfields.minf_inf

  let is_bottom a = (a = BOT)

  let subset a1 a2 = IntBitfields.included_bot a1 a2 

  let join a1 a2 = IntBitfields.join_bot a1 a2

  let meet a1 a2 = IntBitfields.meet_bot a1 a2

  let widen ctx a1 a2 = join a1 a2

  let print printer a = unformat IntBitfields.fprint_bot printer a

  include DefaultValueFunctions

  let constant c t =
    match c with
    | C_bool true             -> Nb IntBitfields.one
    | C_bool false            -> Nb IntBitfields.zero
    | C_int i                 -> Nb (IntBitfields.cst i)
    | C_int_interval (lo, hi) -> IntBitfields.of_bound_bot lo hi
    | _                       -> top

  let unop op t a tr =
    match op with
    | O_log_not    -> bot_lift1 IntBitfields.log_not a
    | O_bit_invert -> bot_lift1 IntBitfields.bit_not a
    | O_plus       -> a
    | _            -> top

  let binop op t1 a1 t2 a2 tr =
    match op with
    | O_bit_and    -> bot_lift2 IntBitfields.bit_and a1 a2
    | O_bit_or     -> bot_lift2 IntBitfields.bit_or a1 a2
    | O_bit_xor    -> bot_lift2 IntBitfields.bit_xor a1 a2
    | O_bit_rshift -> bot_lift2 IntBitfields.shift_right a1 a2
    | O_bit_lshift -> bot_lift2 IntBitfields.shift_left a1 a2
    | O_log_and    -> bot_lift2 IntBitfields.log_and a1 a2
    | O_log_or     -> bot_lift2 IntBitfields.log_or a1 a2
    | _            -> top
  
  let filter b t = function
  | BOT -> BOT
  | Nb a as x when b ->
    if IntBitfields.contains_nonzero a then x else BOT
  | Nb a as x (* not b *) ->
    if IntBitfields.contains_zero a then x else BOT
end

let () =
  register_simplified_value_abstraction (module Value)
