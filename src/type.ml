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

open Unify
open Code
open BatList
open BatPervasives

type signature = {
  input  : U.t list;
  output : U.t list;
}

let void_signature = {
  input = [];
  output = [];
}

type annotation = opcode * signature

type func = string * signature

type dictionary = func list

let basic_type name = U.Term ( name, [ ] )

let int_type = basic_type "int"

let float_type = basic_type "float"

let closure_type {
  input = input;
  output = output;
} =
  U.Term ( "closure",
	   [
	     U.Term ( "input", input );
	     U.Term ( "output", output )
	   ])

exception Type_error of string

let type_error signature signature' =
  let str =
    Printf.sprintf "Expected type `%s', found `%s'!"
      (U.to_string signature)
      (U.to_string signature')
  in
  raise (Type_error str)

let rec check dictionary ( {
  input = input;
  output = output;
} as current ) =

  let rec apply_func =
    function
      | input, [] -> input
      | [], x :: xs -> x :: apply_func ([], xs)
      | x :: xs, y :: ys ->
	if x <> y then type_error x y
	else x :: apply_func (xs, ys)
  in

  function
    | PushInt _     -> { current with output = int_type :: output }
    | PushFloat _   -> { current with output = float_type :: output }
    | PushCode code -> { current with output = closure_type (check_type dictionary current code) :: output }
    | Call name     ->
      let { input = input'; output = output' } = List.assoc name dictionary
      in
	{ input = apply_func (input', output);
	  output = output' }
and check_type dictionary current = 
  List.fold_left (check dictionary) current

let signature_to_string ( {
  input = input;
  output = output;
} as current ) =
  let aux = String.concat " -> " -| List.map U.to_string in
  let input_str = aux input in
  let output_str = aux output in
    String.concat " : " [ input_str; output_str ]

(*
    | App ->
      match output with
	| func :: output ->
	  begin
	    let check
	    match func with
	      | U.Term ( "closure",
			 U.Term ( "input", input' ),
			 U.Term ( "output", output' ) ) ->
		if output <> output' then type_error output output'
		else { current with output = output  input }
*)


(*
let rec basic_type_to_string a_type =
  function
    | U.Term ( name, [ ] ) -> name
    | _ -> failwith "basic_type_to_string"
and arguments_of_string a_types =
  String.concat
and type_to_string

    function
    | U.Term ( "closure",
	       U.Term ( "input", input ),
	       U.Term ( "output", output ) ) -> Printf.printf "closure of type ( %s )" (type_of_string
*)
