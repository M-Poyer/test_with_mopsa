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

(** Callstack - representation of the call stack of a program execution *)

open Location

type callsite = {
  call_fun_orig_name:  string;
  call_fun_uniq_name:  string;
  call_range: range;
}

let pp_callsite fmt c =
  Format.fprintf fmt "%a: %s"
    pp_relative_range c.call_range
    c.call_fun_orig_name

let compare_callsite c c' =
  if c == c' then 0
  else Compare.compose [
    (fun () -> compare c.call_fun_uniq_name c'.call_fun_uniq_name);
    (fun () -> compare_range c.call_range c'.call_range);
  ]

type loop_info =
  | Unrolling of int
  | Fixpoint_iteration

type loopsite = {
  loop_range: range;
  loop_info: loop_info;
}

let pp_loopsite fmt l =
  Format.fprintf fmt "%a: %s" pp_range l.loop_range
    (match l.loop_info with
     | Unrolling n -> Format.asprintf "unrolling %d" n
     | Fixpoint_iteration -> Format.asprintf "fixpoint iteration")

let compare_loopsite l l' =
  if l == l' then 0
  else Compare.compose [
      (fun () -> compare_range l.loop_range l'.loop_range);
      (fun () -> compare l.loop_info l'.loop_info)
    ]

type control_info =
  | Call of callsite
  | Loop of loopsite

let compare_control_info ci ci' =
  match ci, ci' with
  | Call c, Call c' -> compare_callsite c c'
  | Loop l, Loop l' -> compare_loopsite l l'
  | _ -> Stdlib.compare ci ci'

type control_ctx = control_info list

let within_enriched_loop_ctx control_ctx loopsite cont =
  let control_ctx = (Loop loopsite) :: control_ctx in
  cont control_ctx

type callstack = callsite list

let extract_callstack control_ctx =
  List.filter_map (function
      | Call c -> Some c
      | Loop _ -> None) control_ctx

let pp_callstack fmt (cs:callstack) =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt c ->
          Format.fprintf fmt "\tfrom %a" pp_callsite c
       )
    ) cs

let pp_callstack_short fmt cs =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " → ")
    (fun fmt c -> Format.fprintf fmt "%s@%a" c.call_fun_orig_name pp_relative_range c.call_range)
    fmt cs

let compare_callstack cs cs' =
  Compare.list compare_callsite cs cs'

let empty_callstack : callstack = []

let is_empty_callstack = function
  | [] -> true
  | _ -> false

let callstack_length (cs:callstack) : int = List.length cs

let push_callstack orig ?(uniq=orig) range cc =
  Call ({ call_fun_orig_name = orig;
          call_fun_uniq_name = uniq;
          call_range = range }) :: cc

let push_callstack_cs orig ?(uniq=orig) range cc =
  ({ call_fun_orig_name = orig;
     call_fun_uniq_name = uniq;
     call_range = range }) :: cc


exception Empty_callstack

exception Bad_control_ctx

let pop_callstack cc =
  match cc with
  | [] -> raise Empty_callstack
  | Loop _ :: _ -> raise Bad_control_ctx
  | Call c :: tl -> c, tl 

let callstack_top (cs:callstack) : callsite =
  try List.hd cs
  with Failure _ -> raise Empty_callstack

let callstack_begins_with (cs:callstack) (cs':callstack) : bool =
  let n = callstack_length cs in
  let n' = callstack_length cs' in
  if n <= n' then
    false
  else
    let rec aux i x =
      if i = n' then compare_callstack x cs' = 0
      else aux (i-1) (List.tl x)
    in
    aux n cs

let pp_control_ctx fmt control_ctx =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt c ->
          match c with
          | Call cs -> Format.fprintf fmt "\tfrom %a" pp_callsite cs
          | Loop ls -> Format.fprintf fmt "\tfrom %a" pp_loopsite ls
       )
    ) control_ctx

let compare_control_ctx cs cs' =
  Compare.list compare_control_info cs cs'

let empty_control_ctx : control_ctx = []

let is_empty_control_ctx = function
  | [] -> true
  | _ -> false
