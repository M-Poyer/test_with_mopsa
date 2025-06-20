(*
 * This file is part of MOPSA, a Modular Open Platform for Static Analysis.
 *
 * SPDX-FileCopyrightText: 2017-2024 The Mopsa Authors
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

open Instances
open MopsaLib
open Sig.Abstraction.Simplified

let numeric_domain : (module RELATIONAL) ref = ref (module HetPolyhedra : RELATIONAL)

let enable_rel_domain rel_domain_type activate name =
  let (module M : RELATIONAL) = List.find
      (fun (module C : RELATIONAL) -> C.numeric_name = name && C.name = rel_domain_type)
      !numeric_domains in
  if activate then
    let () = opt_numeric := M.numeric_name in
    numeric_domain := (module M : RELATIONAL);
  register_simplified_domain (module M)

let register_rel_domain is_het =
  let suffix = if is_het then "_heterogeneous" else "" in
  let rel_domain_type = "universal.numeric.relational" ^ suffix in
  let key = "-numeric" ^ suffix in
  register_shared_option rel_domain_type {
    key;
    category = "Numeric";
    doc = " select the relational numeric domain.";
    spec = Symbol (
        get_instances_names (),
        enable_rel_domain rel_domain_type true
      );
    default = "polyhedra"
  }


let () =
  let rel_domain_type = "universal.numeric.relational" in
  register_rel_domain false;
  enable_rel_domain rel_domain_type true "polyhedra"

let () =
  let rel_domain_type = "universal.numeric.relational_heterogeneous" in
  register_rel_domain true;
  enable_rel_domain rel_domain_type false "polyhedra"
