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

open BatPervasives

type opcode = 
  | PushInt of int 
  | PushFloat of float 
  | PushBool of bool
  | PushString of string
  | Call of string 
  | PushCode of code
  | App
and code = opcode list
      
let rec to_string = 
  function
    | PushInt v -> Printf.sprintf "%d " v
    | PushFloat v -> Printf.sprintf "%ff " v
    | Call nm -> Printf.sprintf "%s " nm
    | PushCode c -> Printf.sprintf "[ %s ]" **> String.concat " " **> List.map to_string c

let compile = 
  List.map **>
    function 
      | Lexer.Token.Integer v -> PushInt v 
      | Lexer.Token.Float v -> PushFloat v
      | Lexer.Token.Word v -> Call v
