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

(** Policies for grouping heap addresses *)

open Mopsa
open Ast

type addr_partitioning +=
   | G_range of range
   | G_stack_range of callstack * range
   | G_control_ctx_range of control_ctx * range (* also add loop unrolling info *)
   | G_stack of callstack

let () =
  register_addr_partitioning {
      compare = (fun next g1 g2 ->
        match g1, g2 with
        | G_range r, G_range r' ->
           compare_range r r'
        | G_stack_range (cs, r), G_stack_range (cs', r') ->
           Compare.compose
             [ (fun () -> compare_callstack cs cs');
               (fun () -> compare_range r r'); ]
        | G_stack(cs), G_stack(cs') ->
           compare_callstack cs cs'
        | G_control_ctx_range(c, r), G_control_ctx_range(c', r') ->
          Compare.pair compare_control_ctx compare_range (c, r) (c', r')
        | _ -> next g1 g2
      );
      print = (fun next fmt g ->
        match g with
        | G_range r -> pp_relative_range fmt r
        | G_stack_range(cs, r) -> Format.fprintf fmt "%a:%a" pp_callstack_short cs pp_relative_range r
        | G_stack(cs) -> pp_callstack_short fmt cs
        | G_control_ctx_range(c, r) -> Format.fprintf fmt "%a:%a" pp_control_ctx c pp_relative_range r
        | _ -> next fmt g
      );
    }

let mk_addr_range addr_kind addr_mode range cc =
  { addr_kind;
    addr_mode;
    addr_partitioning = G_range range }

let mk_addr_stack_range addr_kind addr_mode range cc =
  { addr_kind;
    addr_mode;
    addr_partitioning = G_stack_range (extract_callstack cc, range) }

let mk_addr_control_range addr_kind addr_mode range cc =
  { addr_kind;
    addr_mode;
    addr_partitioning = G_control_ctx_range (cc, range) }

let mk_addr_stack addr_kind addr_mode range cc =
  { addr_kind;
    addr_mode;
    addr_partitioning = G_stack (extract_callstack cc) }

let mk_addr_all addr_kind addr_mode range cs  =
  { addr_kind; addr_mode; addr_partitioning = G_all }

let mk_addr_chain : (addr_kind -> mode -> range -> control_ctx -> addr) ref =
  ref (fun ak _ _ _ -> assert false)
let mk_addr ak m r cs = !mk_addr_chain ak m r cs
let register_mk_addr f = mk_addr_chain := f !mk_addr_chain

let register_option (opt: string ref) (domain_name: string) (key: string) (descr: string) f =
  register_domain_option domain_name {
      key;
      category = "Heap";
      doc = Format.asprintf " allocation policy used %s" descr;
      spec = Symbol (["all"; "range"; "callstack"; "range_callstack"; "range_control"],
                     (function s -> opt := s; register_mk_addr f));
      default = !opt;
    }

let of_string opt = match opt with
    | "all" -> mk_addr_all
    | "range" -> mk_addr_range
    | "callstack" -> mk_addr_stack
    | "range_callstack" -> mk_addr_stack_range
    | "range_control" -> mk_addr_control_range
    | _ -> panic "unknown policy %s" opt
