(*---------------------------------------------------------------------------- 
nuFourk - Concatenative, stack based, functional, strictly typed language.
Copyright (C) 2010 Wojciech Meyer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------*)

{
module Token = struct
type t = Integer of int | Float of float | Word of string | String of string
let to_string = function
  | Integer v -> string_of_int v
  | Float v -> string_of_float v
  | Word v -> v
  | String v -> "\"" ^ v ^ "\""
end
}

let word = [^' ''\t''\n']+
let float_num = ['-''+']?['0'-'9']+'.'['0'-'9']*(['e''E']['-''+']?['0'-'9']+)?
let int_num = ['-''+']?['0'-'9']+
let whites = [' ''\t''\n']+

rule next_token interpret state =

  parse 
    | whites          { next_token interpret state lexbuf }
    | float_num as t  { let state = interpret state (Token.Float (float_of_string t)) in next_token interpret state lexbuf }
    | int_num as t    { let state = interpret state (Token.Integer (int_of_string t)) in next_token interpret state lexbuf }
    | word as t       { let state = interpret state (Token.Word t) in next_token interpret state lexbuf }
{
  let next_block interpret state = next_token interpret state (Lexing.from_channel stdin)
}
