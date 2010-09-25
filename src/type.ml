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
open BatList.Exceptionless

type atype = 
  | BoolType 
  | IntType
  | FloatType
  | StringType
  | VarType of string
  | ArrowType of signature
and signature = atype list * atype list
    
let void_signature = ([], [])

type annotation = opcode * signature

type func = string * signature

type dictionary = func list

exception Type_error of string

let rec unified_type =
  function
  | BoolType -> U.prim "bool"
  | IntType -> U.prim "int"
  | FloatType -> U.prim "float"
  | StringType -> U.prim "string"
  | VarType name -> U.var name
  | ArrowType (lhs, rhs) -> 
    U.arrow 
      (List.map unified_type lhs) 
      (List.map unified_type rhs)

let type_error signature signature' =
  let str =
    Printf.sprintf "Expected type `%s', found `%s'!"
      (U.to_string signature)
      (U.to_string signature')
  in
  raise (Type_error str)

let prim_signature pushed = [], [pushed]

let rec opcode_to_signatures dictionary =
  let pi = prim_signature in
    function
      | PushBool _    -> pi BoolType
      | PushString _  -> pi StringType
      | App           -> pi (ArrowType ([], []))
      | PushInt _     -> pi IntType
      | PushFloat _   -> pi FloatType
      | PushCode code -> pi (check_code_type code)
      | Call name     -> List.assoc name dictionary
and code_to_signature dictionary = 
  List.map (opcode_to_signatures dictionary)
and check_code_type dictionary = IntType

(*
let rec check dictionary ((input, output) as current) =
  let rec apply_func =
    function
      | input, [] -> input
      | [], x :: xs -> x :: apply_func ([], xs)
      | x :: xs, y :: ys ->
	if x <> y then type_error x y
	else x :: apply_func (xs, ys)
  in
*)
(*
and check_type dictionary current = 
  List.fold_left (check dictionary) current

let rec type_to_string =
  function
  | BoolType -> "bool"
  | IntType -> "int"
  | FloatType -> "float"
  | VarType name -> Printf.sprintf "'%s" name
  | Arrow signature -> Printf.sprintf "(%s)" (signature_to_string signature)

and signature_to_string (lhs,rhs) =
  let concat_types types = 
    String.concat " " (List.map to_string types) in
  let lhs_types = concat_types lhs in
  let rhs_types = concat_types rhs in
    String.concat " -> " [ lhs_types; rhs_types ]
    
type stack_effect = 
  | Accepting of U.t list
  | Leaving of U.t list

  let null_effect = Accepting []
  let rec combine_l x y =
    let rec loop = function
      | x :: xs,y :: ys -> (x,y) :: loop (xs,ys)
      | [], _ -> []
      | _, [] -> []
    in
      loop (x,y)
  let rec check_effects signatures lhs rhs effect =
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
	      unify_types signatures' (combine_l o' i') effect'
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
      match effect with
	| Leaving a -> signatures', { output = effect'; input = [] }
	| Accepting a -> signatures', { output = []; input = effect' }
  let check_pair signatures { input = input1; output = output1 } { input = input2; output = output2 } =
    let signatures', { input = input'; output = output' } = check_effects signatures output1 input2 null_effect 
    in
      signatures', { input = input1 @ input'; output = output2 @ output'  }

  let rec signature_of_code dict opcodes =
    
    let to_signatures lst = 
      let prim_signature t = [], [t] in
      let rec loop acc lst =
	let out = match lst with
	| App::xs -> 
	  loop ({ 
	    input = [U.Term ("code", [U.Term ("list", [U.Var "a"]); U.Term ("list", [U.Var "b"])])];
	    output = [U.Var "b"]} :: acc) xs
      | PushInt _ :: xs   -> (loop (prim_signature IntType)    :: acc) xs 
      | PushFloat _ ::xs  -> (loop (prim_signature FloatType)  :: acc) xs 
      | PushBool _ ::xs   -> (loop (prim_signature BoolType)   :: acc) xs 
      | PushString _ ::xs -> (loop (prim_signature StringType) :: acc) xs 
      | PushCode code::xs ->
	let signature = signature_of_code dict code in
	  loop ({ input = []; output = [U.Term ("code", [U.Term ("list", signature.input); U.Term ("list", signature.output)])] }::acc) xs
      | Call name::xs -> loop ((List.assoc name dict)::acc) xs 
      | [] -> acc
	in
	  sanitase out
      in
	List.rev (loop [] lst) in
    
    let rec rename i = function
      | x :: xs -> { input = List.map (U.rename i) x.input;
		     output = List.map (U.rename i) x.output;} :: rename (i+1) xs
      | [] -> []
    in
    let signatures = rename 0 **> sanitase **> to_signatures opcodes in

    let rec loop signatures previous = function
      | current :: rest -> 
	let signatures', out_signature = check_pair signatures previous current in
	  loop signatures' (sanitase1 out_signature) rest
      | [] -> signatures, previous
    in
      
    let rec type_loop signatures =
      let signatures', sign = 
	match signatures with
	| current :: rest -> loop signatures current rest
 	| [] -> [], void_signature
      in
	if signatures = signatures' then
	  signatures', sanitase1 sign
	else
	  type_loop signatures' 
    in
    let _,{ input = inp; output = out }  = type_loop signatures in
    let rec variable_map ass char = 
      function
	| U.Var nm -> 
	  (match BatList.Exceptionless.assoc nm ass with
	    | Some _ -> char, ass
	    | None -> (char_of_int **> (int_of_char char + 1)), (nm, char)::ass)
	| U.Term (_, lst) -> 
	  List.fold_left 
	    (fun (char,ass) el -> variable_map ass char el) (char, ass) lst
    in
    let rec replace_vars ass =
      function
	| U.Var nm -> U.Var (string_of_char (List.assoc nm ass))
	| U.Term (nm, lst) -> U.Term (nm, List.map (replace_vars ass) lst)
    in
    let rec loop ch ass =
	function
	  | x :: xs -> let ch', ass' = variable_map ass ch x in
			 loop ch' ass' xs
	  | [] -> ch, ass
    in
    let _, ass = loop 'a' [] (inp@out) in
    let inp, out = List.map (replace_vars ass) inp, List.map (replace_vars ass) out in
      { input = inp; output = out }
*)
