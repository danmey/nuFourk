open Code
open Unify

open BatList

type signature = {
  input  : U.t list;
  output : U.t list;
}
    
type annotation = opcode * signature

type func = string * code

type dictionary = func list

let basic_type name = U.Term ( name, [ ] )

let t_int = basic_type "int"

let t_float = basic_type "float"

let closure_type { 
  input = input; 
  output = output;
} = 
  U.Term ( "closure", 
	   U.Term ( "input", input ), 
	   U.Term ( "output", output ) )

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
  function
    | PushInt _     -> { current with input = t_int :: input }
    | PushFloat _   -> { current with input = t_float :: input }
    | Call name     -> check dictionary current (assoc dictionary name)
    | PushCode code -> { current with input = closure_type (check dictionary current code) :: input }
    | App           -> 
      match input with
	| func :: input -> 
	  begin
	    let check
	    match func with
	      | U.Term ( "closure", 
			 U.Term ( "input", input' ), 
			 U.Term ( "output", output' ) ) ->
		if input <> input' then type_error input input
		else { current with input = output  input }

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
