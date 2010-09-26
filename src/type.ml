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
open BatStd
open BatChar
open BatList.Exceptionless

type atype =				(* Basic type kinds defintions  *)
  | BoolType 
  | IntType
  | FloatType
  | StringType
  | VarType of string
  | BigVarType of string
  | KindType of atype list
  | ArrowType of signature
and signature = atype list * atype list	(* Signature is pair of type kinds,
					   consumed list of types and produced list or 
					   types*)

let void_signature = ([], [])

type func = string * signature

type dictionary = func list

exception Type_error of string

(* Convert to internal representation for unification algorithm *)
let rec unified_type = 	
  function
  | BoolType -> U.prim "bool"
  | IntType -> U.prim "int"
  | FloatType -> U.prim "float"
  | StringType -> U.prim "string"
  | VarType name -> U.var name
  | BigVarType name -> U.var name
  | KindType lst -> U.kind (List.map unified_type lst)
  | ArrowType (lhs, rhs) -> 
    U.arrow 
      (List.map unified_type lhs) 
      (List.map unified_type rhs)

(* The same but for signatures *)
let unified_signature (a,b) =
  List.map unified_type a, List.map unified_type b

(* Convert back from unification data structures *)
let rec normal_type =
  function
  | U.Term("bool", [], []) -> BoolType 
  | U.Term("int", [], []) -> IntType 
  | U.Term("float", [], []) -> FloatType 
  | U.Term("string", [], []) -> StringType 
  | U.Var name -> VarType name
  | U.Term ("arrow", lhs, rhs) -> 
    ArrowType (List.map normal_type lhs,
	   List.map normal_type rhs)
  | _ -> failwith (Printf.sprintf "Pattern match: `normal_type'")

(* The same but for signatures *)
let normal_signature (a,b) =
  List.map normal_type a, List.map normal_type b

(* Convert type kind it to printable representation *)
let rec string_of_type =
  function
  | BoolType -> "bool"
  | IntType -> "int"
  | FloatType -> "float"
  | StringType -> "\"string\""
  | VarType name -> Printf.sprintf "'%s" name
  | BigVarType name -> Printf.sprintf "'%s" (uppercase name)
  | KindType lst -> Printf.sprintf "<%s>" **> String.concat " " (List.map string_of_type lst)
  | ArrowType signature -> Printf.sprintf "(%s)" (string_of_signature signature)

(* The same but for signatures *)
and string_of_signature (lhs,rhs) =
  let concat_types types = 
    String.concat " " (List.map string_of_type types) in
  let lhs_types = concat_types lhs in
  let rhs_types = concat_types rhs in
    String.concat " -> " [ lhs_types; rhs_types ]

(* Throw a typing error *)
let type_error signature signature' =
  let str =
    Printf.sprintf "Expected type `%s', found `%s'!"
      (U.to_string signature)
      (U.to_string signature')
  in
  raise (Type_error str)

(* Primitive signature, used by literals *)
let prim_signature pushed = [], [pushed]

type stack_effect = 			(* Represent stack effect *)
  | Accepting of U.t list
  | Leaving of U.t list

(* Resolve effect of two signatures *)
let rec combine_with_effect =
  function
    | [], b -> Accepting b, []
    | a, [] -> Leaving  a, []
    | a :: xs, b :: ys -> 
      let effect, result = combine_with_effect (xs, ys)
      in  
	effect, (a,b) :: result

let rec signature_of_opcode dictionary =
  let pi = prim_signature in
    function
      | PushBool _    -> pi BoolType
      | PushString _  -> pi StringType
      | App           -> pi (ArrowType ([], []))
      | PushInt _     -> pi IntType
      | PushFloat _   -> pi FloatType
      | PushCode code -> pi (ArrowType (signature_of_code dictionary code))
      | Call name     -> List.assoc name dictionary

and code_signatures dictionary = 
  List.map (signature_of_opcode dictionary)

and check_type_effect effect all first second  =
    
  let effect', combined_sign = 
    combine_with_effect (first, second) in

  let subst = 
    List.fold_left 
      (fun subst el -> 
	subst @ U.unify el) 
      [] combined_sign in

  let subst_sig (a,b) =
    U.apply_all subst a, U.apply_all subst b
  in
 let strip_effect =
    function
      | Leaving a -> a
      | Accepting a -> a
  in

  let wrap_effect original a =
    match original with
      | Leaving _ -> Leaving a
      | Accepting _ -> Accepting a
  in


  let all' = List.map subst_sig all in

  let first', second' = List.split combined_sign in
  let first'', second'', effect'' = 
    U.apply_all subst first', 
    U.apply_all subst second',
    wrap_effect effect' (U.apply_all subst (strip_effect effect'))
  in
  let ret = all', effect'', (first'', second'') in
    match subst with
      | [] -> ret
      | _ when all = all' -> ret
      | _ -> 
	check_type_effect effect'' all' first'' second''    

and signature_of_code dict code =
  let null_effect = Accepting [] in
  let effect_singature =
    function
      | Leaving a   -> [], a
      | Accepting a -> a, []
  in

  let check_pair all (l1, r1) (l2, r2) effect =
    let all', effect', (l', r') = check_type_effect effect all r1 l2
    in
    let (l', r') = effect_singature effect' in
      all', effect', (l1 @ l', r2 @ r') in

    let rec pair_loop all effect previous = function
      | current :: rest -> 
	let all', effect', sign = check_pair all previous current effect in
	  pair_loop all' effect' sign rest
      | [] -> effect, all, previous
    in

  let signatures = 
    List.map unified_signature 
      (code_signatures dict code) in


    let rec fold_new_variables (idx, ass) = 
      function
	| U.Var nm ->
	  (match BatList.Exceptionless.assoc nm ass with
	    | Some _ -> idx, ass
	    | None -> idx+1, (nm, idx)::ass)
	| U.Term (_, l, r) -> 
	  let idx, ass = 
	    List.fold_left
	      (fun (idx, ass) el -> 
		fold_new_variables (idx, ass) el) (idx, ass) l in
	  let idx', ass' = 
	    List.fold_left
	      (fun (idx, ass) el -> 
		fold_new_variables (idx, ass) el) (idx, ass) r in
	    idx', ass @ ass'
      in
	
    let rec replace_vars ass =
      let achar nm =
	let idx = (List.assoc nm ass) in
	let char_base = 
	  if BatChar.is_uppercase nm.[0] then 'A' else 'a' in
	let final_char = int_of_char char_base + idx in
	let str = string_of_char -| char_of_int in
	    str final_char
	in
      function
    	| U.Var nm -> U.Var (achar nm)
    	| U.Term (nm, l,r)-> 
	  U.Term (nm, 
		  List.map (replace_vars ass) l, 
		  List.map (replace_vars ass) r)
    in

    let normalize_signature (idx, lst) (l, r) = 
      let idx, vars = List.fold_left fold_new_variables (idx,[]) (l @r ) in
	idx, lst @ [List.map (replace_vars vars) l, List.map (replace_vars vars) r]
    in

(*
    let normalize_signatures lst = snd (List.fold_left normalize_signature ('a', []) lst)
    in
*)
    let rename = 
      snd -| List.fold_left 
	(fun (i, acc) (a, b) ->
	  (i+1), acc @ [List.map (U.rename i) a, List.map (U.rename i) b]) 
	(0,[])
    in

    let rec type_loop signatures =
      let effect, signatures', sign = 
	match signatures with
	  | current :: rest -> pair_loop signatures null_effect void_signature signatures
 	  | [] -> null_effect, [], void_signature
      in
	if signatures = signatures' then
	  signatures', sign
	else
	  type_loop signatures'
    in

    let ending_normalize sign = 
      List.hd (snd (normalize_signature (0,[]) sign)) in
    let _, sign = type_loop (rename signatures) in
      normal_signature (ending_normalize sign)
