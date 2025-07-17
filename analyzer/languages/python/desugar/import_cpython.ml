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

module type CPYTHON_IMPORTER =
sig
  val handle_import : string -> string -> range -> ('a, _) man -> 'a flow -> stmt * 'a flow
end

module Disabled_importer : CPYTHON_IMPORTER =
struct
  let handle_import _ _ range _ flow =
    panic "C disabled, no support for multilanguage C+Python analysis!"
end

let importer : (module CPYTHON_IMPORTER) option ref = ref None

let get_cpython_importer () =
  match !importer with
  | None -> Disabled_importer.handle_import
  | Some s ->
    let module D = (val s : CPYTHON_IMPORTER) in
    D.handle_import
