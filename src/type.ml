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

let signature_to_string {
  input = input;
  output = output;
} =
  let aux = String.concat " -> " -| List.map U.to_string in
  let input_str = aux input in
  let output_str = aux output in
    String.concat " : " [ input_str; output_str ]

type stack_effect = 
  | Accepting of U.t list
  | Leaving of U.t list

  let null_effect = Accepting []

  let rec check_effects l r effect =
    let rec unify_types combined effect =
      let subst = List.fold_left (fun subst el -> subst @ U.unify el) [] combined in
	match subst with
	  | [] -> effect, combined
	  | _ -> 
	    let o, i = List.split combined in
	    let o', i', effect' = U.apply_all subst o, U.apply_all subst i, U.apply_all subst effect in
	      unify_types (List.combine o' i') effect'
    in
    let rec combine a b =
      match a,b with
	| [], b -> Accepting b, []
	| a, [] -> Leaving a, []
	| a::xs, b::ys -> 
	  let effect, result = combine xs ys 
	  in  effect, (a,b) :: result in
    let effect, combined = combine l r in
    let e = match effect with Leaving a -> a | Accepting a -> a in
    let effect', combined' = unify_types combined e in
    let o, i = List.split combined' in
      match effect with
	| Leaving a -> { output = effect'; input = [] }
	| Accepting a -> { output = []; input = effect' }
	  
  let check_pair { input = input1; output = output1 } { input = input2; output = output2 } =
    let { input = input'; output = output' } = check_effects output1 input2 null_effect 
    in
      { input = input1 @ input'; output = output2 @ output'  }

  let signature_of_code dict =
    let of_opcode = function
      | PushInt _ -> { input = []; output = [int_type] }
      | PushFloat _ -> { input = []; output = [float_type] }
      | Call name -> List.assoc name dict
    in
    let rec loop previous = function
      | current :: rest -> loop (check_pair previous (of_opcode current)) rest
      | [] -> previous
    in
      function
	| current :: rest -> loop (of_opcode current) rest
	| [] -> void_signature

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
