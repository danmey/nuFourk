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

let bool_type = basic_type "bool"

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

  let rec check_effects signatures l r effect =
    let rec unify_types signatures combined effect =
      let subst = List.fold_left (fun subst el -> subst @ U.unify el) [] combined in
      let subst_sig ({ input = input; output = output }) =
	{ input = U.apply_all subst input; output = U.apply_all subst output }
      in
      let signatures' = List.map subst_sig signatures in
	match subst with
	  | [] -> signatures', effect, combined
	  | _ when signatures = signatures' -> signatures', effect, combined
	  | _ -> 
	    let o, i = List.split combined in
	    let o', i', effect' = U.apply_all subst o, U.apply_all subst i, U.apply_all subst effect in
	      unify_types signatures' (List.combine o' i') effect'
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
    let signatures', effect', combined' = unify_types signatures combined e in
    let o, i = List.split combined' in
      match effect with
	| Leaving a -> signatures', { output = effect'; input = [] }
	| Accepting a -> signatures', { output = []; input = effect' }
	  
  let check_pair signatures { input = input1; output = output1 } { input = input2; output = output2 } =
    let signatures', { input = input'; output = output' } = check_effects signatures output1 input2 null_effect 
    in
      signatures', { input = input1 @ input'; output = output2 @ output'  }

  let rec signature_of_code dict opcodes =
    
    let to_signatures lst = 
      let rec loop acc = function
	| App::xs -> (match acc with
	    | [] ->  { 
	      input = [U.Term ("code", [U.Term ("input", [U.Var "a"]); U.Term ("output", [U.Var "b"])])];
	      output = [U.Var "b"]} :: acc
	    | s::acc ->
	      (match s with
		  { output = [U.Term ("code", [U.Term ("input", inp); U.Term ("output", out)])];
		    input = _} -> loop ({ input = inp; output = out } :: acc) xs))
      | PushInt _ :: xs -> loop ({ input = []; output = [int_type] }::acc) xs 
      | PushFloat _ ::xs -> loop ({ input = []; output = [float_type] }::acc) xs 
      | PushBool _ ::xs -> loop ({ input = []; output = [bool_type] }::acc) xs 
      | PushCode code::xs -> 
	let signature = snd(signature_of_code dict code) in
	  loop ({ input = []; output = [U.Term ("code", [U.Term ("input", signature.input); U.Term ("output", signature.output)])] }::acc) xs
      | Call name::xs -> loop ((List.assoc name dict)::acc) xs 
      | [] -> acc
      in
	List.rev (loop [] lst) in
(*		

    let of_opcode (prev, acc) el =
      match el with
      | PushInt _ -> [{ input = []; output = [int_type] }], acc@ [{ input = []; output = [int_type] }]
      | PushFloat _ -> [{ input = []; output = [float_type] }], acc@[{ input = []; output = [float_type] }]
      | PushCode code -> 
	let signature = snd(signature_of_code dict code) in
	  , acc @ [{ input = []; output = [U.Term ("code", [U.Term ("input", signature.input); U.Term ("output", signature.output)])] }]
      | Call name -> el, acc @ [List.assoc name dict]
      | App -> print_endline (signature_to_string prev); { input = []; output = [] };
	(match prev with
	  | {
	    output = [U.Term ("code", [U.Term ("input", inp);a])];
	    input = _
	  } -> el, List.rev ({ input = []; output = inp }:: List.rev (List.tl acc)))
    in
*)
    let signatures = to_signatures opcodes in

    let rec loop signatures previous = function
      | current :: rest -> 
	let signatures', out_signature = check_pair signatures previous current in
	  loop signatures' out_signature rest
      | [] -> signatures, previous
    in

    let signatures', sign = 
      match signatures with
	| current :: rest -> loop signatures current rest
	| [] -> [], void_signature
    in
      match signatures' with
	| current :: rest -> loop signatures' current rest
	| [] -> [], void_signature

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
