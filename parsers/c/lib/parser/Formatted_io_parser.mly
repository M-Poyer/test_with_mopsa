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

(** Parser of C stubs *)

%{
  open Formatted_io_placeholder


  let mk_output_placeholder width precision typ =
    let mk w p t = Placeholder {
	op_width = w;
	op_precision = p;
	op_typ = t;
      }
    in
    let width_arg_placeholder = match snd width with
      | true -> [mk None None (Int UNSIGNED_INT)]
      | _ -> []
    in
    let precision_arg_placeholder = match snd precision with
      | true -> [mk None None (Int UNSIGNED_INT)]
      | _ -> []
    in
    width_arg_placeholder @ precision_arg_placeholder @ [mk (fst width) (fst precision) typ]


  let mk_input_placeholder width typ =
    {
      ip_width = width;
      ip_typ = typ;
    }

%}

%token PLUS MINUS ZERO SHARP SPACE
%token <int> NUM
%token STAR DOT
%token H HH L LL CAP_L
%token D I U F G E A P S WS X O C
%token EOF
%token <string> TOK_string

%start parse_output_format
%start parse_input_format

%type <output_format list> parse_output_format
%type <input_placeholder list> parse_input_format

%type<input_placeholder> input_placeholder
%type<input_placeholder list> input_placeholder_list
%type<placeholder_type> input_typ
%type<int option> input_width
%type<unit> output_flag output_flag_list
%type<output_format list> output_format output_format_list
%type<int option * bool> output_precision
%type<placeholder_type> output_typ
%type<int option * bool> output_width

%%


(** Parser of output formats *)

parse_output_format:
  |  l=output_format_list EOF { l }

output_format_list:
  | { [] }
  | hd=output_format tl=output_format_list { hd @ tl }

output_format:
  | v=TOK_string { [String v] }
  | output_flag_list w=output_width p=output_precision t=output_typ { mk_output_placeholder w p t }


output_flag_list:
  | { }
  | output_flag output_flag_list { }

output_flag:
  | PLUS  {  }
  | MINUS {  }
  | ZERO  {  }
  | SHARP {  }
  | SPACE {  }

output_width:
  | n=NUM { Some n, false }
  | STAR  { None, true }
  |       { None, false }

output_precision:
  | DOT n=NUM { Some n, false }
  | DOT       { Some 0, false }
  | DOT STAR  { None, true }
  |           { None, false }

output_typ:
  (* Char *)
  | C                      { Int UNSIGNED_CHAR }

  (* Signed integers *)
  | HH D | HH I            { Int SIGNED_CHAR }
  | H D  | H I             { Int SIGNED_SHORT }
  | D    | I               { Int SIGNED_INT }
  | L D  | L I             { Int SIGNED_LONG }
  | LL D | LL I            { Int SIGNED_LONG_LONG }

  (* Unsigned integers *)
  | HH U | HH X | HH O     { Int UNSIGNED_CHAR }
  | H U  | H X  | H O      { Int UNSIGNED_SHORT }
  | U    | X    | O        { Int UNSIGNED_INT }
  | L U  | L X  | L O      { Int UNSIGNED_LONG }
  | LL U | LL X | LL O     { Int UNSIGNED_LONG_LONG }

  (* Floats *)
  | E | F | G | A          { Float FLOAT }
  | CAP_L E | CAP_L F | CAP_L G | CAP_L A  { Float LONG_DOUBLE }

  (* String and pointers *)
  | S                      { String }
  | WS                     { WideString }
  | P                      { Pointer }




(** Parser of input format *)

parse_input_format:
  | l=input_placeholder_list EOF { l }

input_placeholder_list:
  | { [] }
  | TOK_string tl=input_placeholder_list { tl }
  | STAR input_placeholder tl=input_placeholder_list { tl }
  | hd=input_placeholder tl=input_placeholder_list   { hd :: tl }

input_placeholder:
  | w=input_width t=input_typ { mk_input_placeholder w t }

input_width:
  | n=NUM { Some n }
  |       { None }

input_typ:
  (* Char *)
  | C                      { Int UNSIGNED_CHAR }

  (* Signed integers *)
  | HH D | HH I            { Int SIGNED_CHAR }
  | H D  | H I             { Int SIGNED_SHORT }
  | D    | I               { Int SIGNED_INT }
  | L D  | L I             { Int SIGNED_LONG }
  | CAP_L D | CAP_L I      { Int SIGNED_LONG_LONG }

  (* Unsigned integers *)
  | HH U | HH X | HH O           { Int UNSIGNED_CHAR }
  | H U  | H X  | H O            { Int UNSIGNED_SHORT }
  | U    | X    | O              { Int UNSIGNED_INT }
  | L U  | L X  | L O            { Int UNSIGNED_LONG }
  | CAP_L U | CAP_L X | CAP_L O  { Int UNSIGNED_LONG_LONG }

  (* Floats *)
  | E | F | G | A                          { Float FLOAT }
  | L E | L F | L G | L A                  { Float DOUBLE }
  | CAP_L E | CAP_L F | CAP_L G | CAP_L A  { Float LONG_DOUBLE }

  (* String and pointers *)
  | S                      { String }
  | P                      { Pointer }
