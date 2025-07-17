(*
 * This file is part of MOPSA, a Modular Open Platform for Static Analysis.
 *
 * Copyright (C) 2025 The MOPSA authors.
 *
 * SPDX-License-Identifier: LGPL-3.0-or-later
 *
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 *)

open MopsaLib

module Enabled_importer : Python.Desugar.Import_cpython.CPYTHON_IMPORTER =
struct

  let handle_import probable_name filename range man flow =
    let () =
      C.Lang.Frontend.opt_make_target := probable_name in
    let prog = C.Lang.Frontend.parse_program [filename] in
    (* let () = debug "Parsed C program %a" pp_program prog in *)
    let () = C.Iterators.Program.Domain.opt_entry_function := "PyInit_" ^ probable_name in
    (* panic_at range "c.coverage hook mem: %b" (Hook.mem_hook "c.coverage"); *)
    let () = debug "Searching for entry function %s" !C.Iterators.Program.Domain.opt_entry_function
    (* (Format.pp_print_list (fun fmt fdec -> Format.pp_print_string fmt fdec.C.Ast.c_func_org_name)) (match prog.prog_kind with | C.Ast.C_program c -> c.c_functions | _ -> assert false) *)
    in
    let flow = C.Iterators.Program.Domain.init prog man flow |> Option.get |> post_to_flow man in
    let () =
      if Hook.is_hook_active "c.coverage" then
        C.Hooks.Coverage.Hook.init (Flow.get_ctx flow) in
    let body = mk_stmt (S_program(prog, None)) range in
    body, flow

end


let () = Python.Desugar.Import_cpython.importer := Some (module Enabled_importer)
