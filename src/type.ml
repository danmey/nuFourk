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

let string_type = basic_type "string"

let closure_type {
  input = input;
  output = output;
} =
  U.Term ( "closure",
	   [
	     U.Term ( "list", input );
	     U.Term ( "list", output )
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

    let rec sanitaze_u = function
      | U.Var n -> U.Var n 
      | U.Term(n, lst) when n <> "code" -> 
	(match lst with
	  | [ U.Term("list", lst')] -> U.Term(n, lst')
	  | x -> U.Term(n, x))
      | U.Term(n, lst) -> U.Term(n, List.map sanitaze_u lst)

    let rec sanitase1 = function
      | { output = o; input = i } -> { output = List.map sanitaze_u o; input = List.map sanitaze_u i}

    let sanitaze_u a = a
    let sanitase1 a = a
let rec to_string u =
  match u with
    | U.Term ("code", [U.Term("list", l1);U.Term("list", l2)]) -> Printf.sprintf "(%s)" (tos l1 l2)
    | U.Term ("code", [l1;l2]) -> Printf.sprintf "(%s)" (tos [l1] [l2])
    | U.Term ("list", l) -> Printf.sprintf "%s" (aux l) 
    | U.Term (nm, l) when List.length l == 0 -> Printf.sprintf "%s" nm
    | U.Term (nm, l) ->
	Printf.sprintf "(%s %s)" nm (aux l)
    | U.Var (nm) -> Printf.sprintf "%s'" nm
and aux u = String.concat " " **> List.map to_string u
and tos input output =
  let input_str = aux input in
  let output_str = aux output in
    String.concat " -> " [ input_str; output_str ]


let signature_to_string {
  input = input;
  output = output;
} = Printf.sprintf "%s\n%s ---- %s" (tos input output) "" ""
  (* (String.concat "\n" (List.map U.to_string input)) (String.concat "\n" (List.map U.to_string output)) *)
  
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
  let rec check_effects signatures l r effect =
    let l = List.map sanitaze_u l in
    let r = List.map sanitaze_u r in
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
    let o, i = List.split combined' in
      match effect with
	| Leaving a -> signatures', { output = effect'; input = [] }
	| Accepting a -> signatures', { output = []; input = effect' }
  let check_pair signatures { input = input1; output = output1 } { input = input2; output = output2 } =
    let signatures', { input = input'; output = output' } = check_effects signatures output1 input2 null_effect 
    in
      signatures', { input = input1 @ input'; output = output2 @ output'  }

  let rec signature_of_code dict opcodes =
    (* Cheating, dirty hack! *)
    let sanitase = List.map 
	(function
	  | { output = [U.Term ("list", x)];
	      input = inp } -> { output = x; input = inp} 
	  | x -> x
	) in

    
    let to_signatures lst = 
      let rec loop acc lst =
	let out = match lst with
	| App::xs -> 
	  loop ({ 
	    input = [U.Term ("code", [U.Term ("list", [U.Var "a"]); U.Term ("list", [U.Var "b"])])];
	    output = [U.Var "b"]} :: acc) xs
      | PushInt _ :: xs -> loop ({ input = []; output = [int_type] }::acc) xs 
      | PushFloat _ ::xs -> loop ({ input = []; output = [float_type] }::acc) xs 
      | PushBool _ ::xs -> loop ({ input = []; output = [bool_type] }::acc) xs 
      | PushString _ ::xs -> loop ({ input = []; output = [string_type] }::acc) xs 
      | PushCode code::xs ->
	let signature = signature_of_code dict code in
	  loop ({ input = []; output = [U.Term ("code", [U.Term ("list", signature.input); U.Term ("list", signature.output)])] }::acc) xs
      | Call name::xs -> loop ((List.assoc name dict)::acc) xs 
      | [] -> acc
	in
	  sanitase out
      in
	List.rev (loop [] lst) in
(*		

    let of_opcode (prev, acc) el =
      match el with
      | PushInt _ -> [{ input = []; output = [int_type] }], acc@ [{ input = []; output = [int_type] }]
      | PushFloat _ -> [{ input = []; output = [float_type] }], acc@[{ input = []; output = [float_type] }]
      | PushCode code -> 
	let signature = snd(signature_of_code dict code) in
	  , acc @ [{ input = []; output = [U.Term ("code", [U.Term ("list", signature.input); U.Term ("list", signature.output)])] }]
      | Call name -> el, acc @ [List.assoc name dict]
      | App -> print_endline (signature_to_string prev); { input = []; output = [] };
	(match prev with
	  | {
	    output = [U.Term ("code", [U.Term ("list", inp);a])];
	    input = _
	  } -> el, List.rev ({ input = []; output = inp }:: List.rev (List.tl acc)))
    in
*)
    
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
			 

