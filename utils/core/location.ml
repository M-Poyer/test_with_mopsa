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

(** Positions and ranges *)


(** {2 Positions} *)
(** ============= *)

type pos = {
  pos_file: string; (** File name. *)
  pos_line: int; (** Line number. *)
  pos_column: int; (** Column number. *)
}
(** Position in a file. *)


let get_pos_file p = p.pos_file
let get_pos_line p = p.pos_line
let get_pos_column p = p.pos_column

let mk_pos file line column =
  {pos_file = file; pos_line = line; pos_column = column}

(** Comparison function of positions. *)
let compare_pos (pos1: pos) (pos2: pos) =
  if pos1 == pos2 then 0
  else Compare.compose [
    (fun () -> compare pos1.pos_file pos2.pos_file);
    (fun () -> compare pos1.pos_line pos2.pos_line);
    (fun () -> compare pos1.pos_column pos2.pos_column);
  ]

(** Return the relative path of `file` w.r.t. the current working directory *)
let relative_path file =
  let wd = Sys.getcwd () in
  if Str.string_match (Str.regexp ("^" ^ (Str.quote wd) ^ "/.+")) file 0 then
    let n1 = String.length wd in
    let n2 = String.length file in
    "./" ^ String.sub file (n1 + 1) (n2 - n1 - 1)
  else
    file


type epos = {
  epos_pos: pos;
  epos_stack: pos list;
}
(** Extended source locations, decorated with a stack of positions.
    It can be used to distinguish between different use of the same lexical
    token.
    E.g., in C, it tracks macro argument expansion.
*)


(** {2 Ranges} *)
(** ========== *)


(** Location range of AST nodes. *)
type range =
  | R_program of string list (** list of source files *)
  (** Program range covering a list source files *)

  | R_orig of pos (** start position. *) * pos (** end position *)
  (** Original source range with a start and an end positions. *)

  | R_ext_orig of epos (** start position *) * epos (** end position *)
  (** As R_orig, but with extended locations. *)

  | R_fresh of int (** non-original fresh range with unique id *)
  (** Fresh ranges with unique identifiers *)

  | R_tagged of range_tag * range
  (** Tagged range with an annotation *)


(** Range tags can be used to annotate AST nodes added by the abstract
   domains that are not textually present in the source files. *)
and range_tag =
  | String_tag of string
  | Range_tag  of range



let mk_tagged_range tag range = R_tagged(tag,range)

let mk_string_tag fmt =
  Format.kasprintf (fun tag ->
      String_tag tag
    ) fmt

let mk_range_tag range = Range_tag range

let mk_range_tagged_range rtag range = R_tagged (mk_range_tag rtag, range)

(** Tag a range with a (formatted) annotation. *)
let tag_range range fmt =
  Format.kasprintf (fun tag ->
      R_tagged (String_tag tag, range)
    ) fmt

let mk_orig_range pos1 pos2 = R_orig (pos1, pos2)
let mk_ext_orig_range pos1 pos2 = R_ext_orig (pos1, pos2)

let fresh_range_counter = ref 0

let mk_fresh_range () =
  incr fresh_range_counter;
  R_fresh !fresh_range_counter

let mk_program_range pl = R_program pl

let rec untag_range = function
  | R_tagged(_, range) -> untag_range range
  | range -> range

let rec map_tag f = function
  | R_tagged (t, r) -> R_tagged (t, map_tag f r)
  | r -> f r

let remove_ext = function
  | R_ext_orig (l1, l2) -> R_orig (l1.epos_pos, l2.epos_pos)
  | x -> x

let get_range_start r =
  match untag_range r with
  | R_orig(pos, _) -> pos
  | R_ext_orig(pos, _) -> pos.epos_pos
  | R_program[p] -> mk_pos p 0 0
  | _ -> failwith "get_range_start: invalid argument"

let get_range_end r =
  match untag_range r with
  | R_orig(_, pos) -> pos
  | R_ext_orig(_, pos) -> pos.epos_pos
  | _ -> failwith "get_range_end: invalid argument"

let set_range_start r l =
  map_tag (fun r ->
      match r with
      | R_orig (_, l') -> R_orig (l, l')
      | R_ext_orig (_, l') -> R_ext_orig ({ epos_pos = l; epos_stack = []; }, l')
      | _ -> failwith "set_range_start: called on non R_source"
    ) r

let set_range_end r l' =
  map_tag (fun r ->
      match r with
      | R_orig (l, _) -> R_orig (l, l')
      | R_ext_orig (l, _) -> R_ext_orig (l, { epos_pos = l'; epos_stack = []; })
      | _ -> failwith "set_range_end: called on non R_source"
    ) r

let get_range_file r =
  match untag_range r with
  | R_orig(pos, _) -> pos.pos_file
  | R_ext_orig(pos, _) -> pos.epos_pos.pos_file
  | R_program [p] -> p
  | _ -> failwith "get_range_file: invalid argument"

let get_range_relative_file r =
  get_range_file r |>
  relative_path

let get_range_line r =
  let pos = get_range_start r in
  pos.pos_line

let get_range_column r =
  let pos = get_range_start r in
  pos.pos_column

let is_orig_range = function
  | R_orig _ | R_ext_orig _ -> true
  | _ -> false

let is_program_range = function
  | R_program _ -> true
  | _ -> false

let match_range_file file r =
  let pred f =
    Str.string_match (Str.regexp (".*" ^ (Str.quote file) ^ "$")) f 0 ||
    Str.string_match (Str.regexp (".*" ^ (Str.quote f) ^ "$")) file 0
  in
  match untag_range r with
  | R_orig(p, _) -> pred p.pos_file
  | R_ext_orig(p, _) -> pred p.epos_pos.pos_file
  | R_program pl -> List.exists pred pl
  | _ -> false

let match_range_line line r =
  match untag_range r with
  | R_orig(p, _) -> p.pos_line = line
  | R_ext_orig(p, _) -> p.epos_pos.pos_line = line
  | _ -> false

let from_lexing_pos pos =
  let open Lexing
  in {
    pos_file = pos.pos_fname;
    pos_line = pos.pos_lnum;
    pos_column = pos.pos_cnum - pos.pos_bol;
  }

let from_lexing_range pos1 pos2 =
  mk_orig_range (from_lexing_pos pos1) (from_lexing_pos pos2)


(** Comparison function of ranges. *)
let rec compare_range (r1: range) (r2: range) =
  if r1 == r2 then 0
  else match r1, r2 with
    | R_program pl1, R_program pl2 ->
      Compare.list compare pl1 pl2

    | R_orig (l1, l2), R_orig (l1', l2') ->
      Compare.compose [
        (fun () -> compare_pos l1 l1');
        (fun () -> compare_pos l2 l2');
      ]

    | R_ext_orig (l1, l2), R_ext_orig (l1', l2') ->
      Compare.compose [
        (fun () -> compare_pos l1.epos_pos l1'.epos_pos);
        (fun () -> compare_pos l2.epos_pos l2'.epos_pos);
        (fun () -> Compare.list compare_pos l1.epos_stack l1'.epos_stack);
        (fun () -> Compare.list compare_pos l2.epos_stack l2'.epos_stack);
      ]

    | R_tagged(t1, r1), R_tagged(t2, r2) ->
      Compare.compose [
        (fun () -> compare_range r1 r2);
        (fun () -> compare_range_tag t1 t2)
      ]

    | R_fresh(uid1), R_fresh(uid2) -> compare uid1 uid2

    | _ -> compare r1 r2

and compare_range_tag tag1 tag2 =
  match tag1, tag2 with
  | String_tag s1, String_tag s2 -> compare s1 s2
  | Range_tag r1, Range_tag r2 -> compare_range r1 r2
  | _ -> compare tag1 tag2


let subset_range (r1:range) (r2:range) : bool =
  if r1 == r2 then true
  else match remove_ext (untag_range r1), remove_ext (untag_range r2) with
    | R_program pl1, R_program pl2 -> Compare.list compare pl1 pl2 = 0
    | R_fresh(uid1), R_fresh(uid2) -> uid1 = uid2
    | R_orig (l1, l2), R_orig (l1', l2') ->
      l1.pos_file = l1'.pos_file
      && l2.pos_file = l2'.pos_file
      && (l1.pos_line > l1'.pos_line
          || (l1.pos_line = l1'.pos_line
              && l1.pos_column >= l1'.pos_column))
      && (l2.pos_line < l2'.pos_line
          || (l2.pos_line = l2'.pos_line
              && l2.pos_column <= l2'.pos_column))
    | _ -> false


(** {2 Range annotations} *)
(** ===================== *)

type 'a with_range = {
  content: 'a;
  range: range;
}

let with_range a range =
  {
    content = a;
    range;
  }

let get_content a = a.content

let get_range a = a.range

let bind_range (a: 'a with_range) ?(range=a.range) (f: 'a -> 'b) : 'b with_range =
  { content = f a.content; range }

let bind_pair_range (a: 'a with_range) (f: 'a -> 'b * 'c) : 'b with_range * 'c =
  let b, c = f a.content in
  { content = b; range = a.range }, c

let compare_with_range cmp a b =
  Compare.compose [
    (fun () -> compare_range a.range b.range);
    (fun () -> cmp a.content b.content);
  ]

(** {2 Pretty printers} *)
(** =================== *)


let pp_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" pos.pos_file pos.pos_line pos.pos_column

let pp_relative_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" (relative_path pos.pos_file) pos.pos_line pos.pos_column

let pp_position_stack pp fmt stack =
  if stack <> [] then (
    Format.fprintf fmt "[";
    let first = ref true in
    List.iter (
      fun pos ->
        if !first then first := false else Format.fprintf fmt ";";
        pp fmt pos
    ) stack;
    Format.fprintf fmt "]"
  )

let pp_eposition fmt epos =
  pp_position fmt epos.epos_pos;
  pp_position_stack pp_position fmt epos.epos_stack

let pp_relative_eposition fmt epos =
  pp_relative_position fmt epos.epos_pos;
  pp_position_stack pp_relative_position fmt epos.epos_stack

let rec pp_range fmt range =
  match range with
  | R_program pl ->
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) pl

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file
                          && pos1.pos_line == pos2.pos_line
                          && pos1.pos_column == pos2.pos_column ->
    pp_position fmt pos1

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file
                          && pos1.pos_line == pos2.pos_line ->
    Format.fprintf fmt "%s:%d.%d-%d"
      pos1.pos_file
      pos1.pos_line
      pos1.pos_column pos2.pos_column

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file ->
    Format.fprintf fmt "%s:%d.%d-%d.%d"
      pos1.pos_file
      pos1.pos_line pos1.pos_column
      pos2.pos_line pos2.pos_column

  | R_orig (pos1, pos2) ->
    Format.fprintf fmt "%a-%a"
      pp_position pos1
      pp_position pos2

  | R_ext_orig (pos1, pos2) when pos1.epos_stack = pos2.epos_stack ->
    pp_range fmt (R_orig (pos1.epos_pos, pos2.epos_pos));
    pp_position_stack pp_position fmt pos1.epos_stack

  | R_ext_orig (pos1, pos2) ->
    Format.fprintf fmt "%a-%a"
      pp_eposition pos1
      pp_eposition pos2

  | R_fresh uid -> Format.fprintf fmt "<%d>" uid

  | R_tagged (String_tag t, r) -> Format.fprintf fmt "%a::%s" pp_range r t

  | R_tagged (Range_tag rr, r) -> Format.fprintf fmt "%a:$%a" pp_range r pp_range rr


let rec pp_relative_range fmt range =
  (* keep tagged range, pp_relative_range is used to generate addr partitioning unique names *)
  match range with
  | R_program pl ->
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) pl

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file
                          && pos1.pos_line == pos2.pos_line
                          && pos1.pos_column == pos2.pos_column ->
    pp_relative_position fmt pos1

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file
                          && pos1.pos_line == pos2.pos_line ->
    Format.fprintf fmt "%s:%d.%d-%d"
      (relative_path pos1.pos_file)
      pos1.pos_line
      pos1.pos_column pos2.pos_column

  | R_orig (pos1, pos2) when pos1.pos_file == pos2.pos_file ->
    Format.fprintf fmt "%s:%d.%d-%d.%d"
      (relative_path pos1.pos_file)
      pos1.pos_line pos1.pos_column
      pos2.pos_line pos2.pos_column

  | R_orig (pos1, pos2) ->
    Format.fprintf fmt "%a-%a"
      pp_relative_position pos1
      pp_relative_position pos2

  | R_ext_orig (pos1, pos2) when pos1.epos_stack = pos2.epos_stack ->
    pp_relative_range fmt (R_orig (pos1.epos_pos, pos2.epos_pos));
    pp_position_stack pp_relative_position fmt pos1.epos_stack

  | R_ext_orig (pos1, pos2) ->
    Format.fprintf fmt "%a-%a"
      pp_relative_eposition pos1
      pp_relative_eposition pos2

  | R_fresh uid -> Format.fprintf fmt "<%d>" uid

  | R_tagged (String_tag t, r) -> Format.fprintf fmt "%a::%s" pp_relative_range r t

  | R_tagged (Range_tag rr, r) -> Format.fprintf fmt "%a:$%a" pp_relative_range r pp_relative_range rr
