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

(** Control context - representation of the call stack and loop analysis positino of a program analysis *)

open Location

(** {1 Call sites} *)
(** ************** *)

type callsite = {
  call_fun_orig_name:  string; (** Original name of the called function *)
  call_fun_uniq_name:  string; (** Unique name of the called function *)
  call_range: range;           (** Call location *)
}
(** Call site is the location of a call in the program *)

val pp_callsite : Format.formatter -> callsite -> unit
(** Print a call site *)

val compare_callsite : callsite -> callsite -> int
(** Compare two call sites *)


(** {1 Loop sites} *)
(** ************** *)

type loop_info =
  | Unrolling of int
  | Fixpoint_iteration

type loopsite = {
  loop_range: range;
  loop_info: loop_info;
}

(** {2 Control context} *)
(** *************** *)

type control_info =
  | Call of callsite
  | Loop of loopsite

type control_ctx = control_info list

val within_enriched_loop_ctx : control_ctx -> loopsite -> (control_ctx -> 'a) -> 'a

type callstack = callsite list

val extract_callstack : control_ctx -> callstack

val pp_callstack : Format.formatter -> callstack -> unit
(** Print a call stack *)

val pp_callstack_short : Format.formatter -> callstack -> unit
(** Print a call stack in a short style *)

val compare_callstack : callstack -> callstack -> int
(** Compare two call stacks *)

val empty_callstack : callstack
(** Empty call stack *)

val is_empty_callstack : callstack -> bool
(** Check that a call stack is empty *)

val callstack_length : callstack -> int
(** Return the length of a call stack *)

val push_callstack : string -> ?uniq:string -> range -> control_ctx -> control_ctx
(** [push_callstack orig ~uniq range cs] adds the call to function
    [orig] at location [range] at the top of the call stack [cs].
    The default unique function name of the function is its original name. *)

val push_callstack_cs : string -> ?uniq:string -> range -> callstack -> callstack
(** Similar as [push_callstack] but works on callstacks only. Useful for variables depending on callstacks only *)


exception Empty_callstack
(** Exception raised when a call stack is empty *)

exception Bad_control_ctx
(** Exception raised when a Call is expected at the top of the control context and a Loop is found (or conversely) *)

val pop_callstack : control_ctx -> callsite * control_ctx
(** [pop_callstack cc] returns the last call site in [cc] and the remaining control context
    Raises [Empty_callstack] if the call stack is empty.
    Raises [Bad_control_ctx] if the control context starts with Loop info.
*)

val callstack_top : callstack -> callsite
(** [callstack_top cs] returns the last call site in [cs].
    Raises [Empty_callstack] if the call stack is empty. *)

val callstack_begins_with : callstack -> callstack -> bool
(** [callstack_begins_with ext cs] checks that [ext] is an extension of [cs], i.e. ext = x @ cs *)

val pp_control_ctx : Format.formatter -> control_ctx -> unit
(** Print control context *)

val compare_control_ctx : control_ctx -> control_ctx -> int
(** Compare control context *) 

val empty_control_ctx : control_ctx
(** Empty control context *)

val is_empty_control_ctx : control_ctx -> bool
(** Check that a control context is empty *)
