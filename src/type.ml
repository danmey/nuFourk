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
					   consumed list of types and produced 
					   list or types*)

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
  | BigVarType name -> U.var (uppercase name)
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
    | U.Term("bool", []) -> BoolType 
    | U.Term("int", []) -> IntType 
    | U.Term("float", []) -> FloatType 
    | U.Term("string", []) -> StringType 
    | U.Var name -> if is_uppercase name.[0] then BigVarType name else VarType name
    | U.Term ("kind", lst) -> KindType (List.map normal_type lst)
    | U.Term ("arrow", [U.Term ("in", lhs); U.Term ("out", rhs)]) -> 
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
    | StringType -> "string"
    | VarType name -> Printf.sprintf "%s" name
    | BigVarType name -> Printf.sprintf "%s" (uppercase name)
    | KindType lst -> Printf.sprintf "<%s>" **> String.concat " " (List.map string_of_type lst)
    | ArrowType signature -> Printf.sprintf "(%s)" (string_of_signature signature)

(* The same but for signatures *)
and string_of_signature (lhs,rhs) =
  let concat_types =
    function
      | [] -> "()"
      | types ->    
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
  let merge_effect = function
    | Accepting a, Leaving b ->
      let rec loop = function
        | x::xs, y::ys -> loop (xs,ys)
        | [], ys -> Leaving ys
        | xs, [] -> Accepting xs
        | [], [] -> Accepting [] in
      loop (a,b)
    | Leaving a, Accepting b ->
      let rec loop = function
        | x::xs, y::ys -> loop (xs,ys)
        | [], ys -> Accepting ys
        | xs, [] -> Leaving xs
        | [], [] -> Accepting [] 
      in
      loop (a,b)
    | Accepting a, Accepting b -> Accepting (a@b)
    | Leaving a, Leaving b -> Leaving (a@b)
  in
  
  let rec loop_unif effect subst = function
    | [],[] -> 
      let subst, effect = 
        (try
           let l = List.assoc "right" subst in
           subst, merge_effect (effect, Accepting [l])
      with Not_found -> subst, effect) in
      let subst, effect =
        (try
           let l = List.assoc "left" subst in
           subst, merge_effect (effect, Leaving [l])
      with Not_found -> subst, effect) in
      subst, effect
    | el::xs,y::ys  ->
      (* print_endline "ala"; *)
      (match el with
        (* | U.Term("kind", lst) -> *)
        (*   (\* List.iter (print_endline -| U.to_string) lst; *\) *)
        (*   let n = List.length lst - 1 in *)
        (*   let newlst = el :: take n ys in *)
        (*   let effect =  *)
        (*     if List.length newlst < List.length lst then *)
        (*       Accepting (drop (List.length newlst) lst) *)
        (*     else Accepting [] in *)
        (*   let zipped = List.combine newlst lst in *)
        (*   let subst = List.fold_left *)
        (*     (fun subst el -> *)
        (*       subst @ U.unify el) subst zipped in *)
        (*   loop_unif effect subst ([],[]) *)
        | _ -> loop_unif effect (subst @ U.unify (el,y)) (xs,ys))
  in
  (* let subst =  *)
  (*   List.fold_left  *)
  (*     (fun subst el ->  *)
  (*       print_endline "**************"; *)
  (*       match el with *)
  (*         | Term("kind", lst) -> *)
  (*       subst @ U.unify el)  *)
  (*     [] combined_sign in *)
  let subst, effect2 = loop_unif (Accepting []) [] (List.split combined_sign) in
  let effect' = merge_effect (effect', effect2) in
  let rec loop = function
    | (nm,v)::xs -> 
      let v2 = 
	try 
	  List.assoc nm xs 
	with 
	    Not_found -> loop xs
      in 
	ignore(U.unify (v,v2)); loop xs
    | [] -> U.Var "a"  
  in
    loop subst;
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
  let ret = all', effect'', (first'', second''), subst in
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
    let all', effect', (l', r'), subst = check_type_effect effect all r1 l2
    in
    let (l', r') = effect_singature effect' in
      all', effect', (l1 @ l', r2 @ r'), subst in

    let rec pair_loop subst all effect previous = function
      | current :: rest -> 
	let all', effect', sign, subst = check_pair all previous current effect in
	  pair_loop subst all' effect' sign rest
      | [] -> effect, all, previous, subst
    in

    let rec fold_new_variables (idx, ass) = 
      function
	| U.Var nm ->
	  (match BatList.Exceptionless.assoc nm ass with
	    | Some _ -> idx, ass
	    | None -> idx+1, (nm, idx)::ass)
	| U.Term (_, l) -> 
	    List.fold_left
	      (fun (idx, ass) el -> 
		fold_new_variables (idx, ass) el) (idx, ass) l in
	
    let rec replace_vars ass =
      let achar nm =
	let idx = (List.assoc nm ass) in
	let char_base = 
          (* print_endline nm; *)
          flush stdout;
	  if BatChar.is_uppercase nm.[0] then 'A' else 'a' in
	let final_char = int_of_char char_base + idx in
	let str = string_of_char -| char_of_int in
	    str final_char
	in
      function
    	| U.Var nm -> U.Var (achar nm)
    	| U.Term (nm, l)-> 
	  U.Term (nm, 
		  List.map (replace_vars ass) l)
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

    let rec type_loop subst effect signatures =
      let effect', signatures', sign,subst = 
	match signatures with
	  | current :: rest -> pair_loop [] signatures null_effect void_signature signatures
 	  | [] -> null_effect, [], void_signature, subst
      in
	if signatures = signatures' then
	  signatures', sign, subst
	else
	  type_loop subst effect' signatures'
    in
      
    
    let ending_normalize sign = 
      List.hd (snd (normalize_signature (0,[]) sign)) in


    let rec to_kind_type lst =
      let rec loop = 
	function
	  | ArrowType (l, r) -> ArrowType (big_var_check l, big_var_check r)
	  | a -> a
      and big_var_check = 
	function
	  | BigVarType nm :: xs -> [BigVarType nm] @ [KindType (List.map loop xs)]
	  | xs -> List.map loop xs
      in
	List.map loop lst
    in
    let to_kind_type' = to_kind_type *** to_kind_type in
    
    let type_signatures = code_signatures dict code in

    let signatures = 
      List.map unified_signature type_signatures in

    let signatures' = rename signatures in
(*      
    let signatures, sign = type_loop null_effect signatures' in
    let signatures = List.map normal_signature signatures in
    let type_signatures' = List.map to_kind_type' signatures in
    let signatures' = 
      List.map unified_signature type_signatures' in
*)
    let rec remove_kind = function
      | U.Term (nm, [U.Term ("kind", lst)]) -> U.Term (nm, List.map remove_kind lst)
      | a -> a
    in
    let remove_kind_top = function
      | U.Term ("kind", lst) -> lst
      | a -> [a] in
    let signatures, sign,subst = type_loop [] null_effect signatures' in
    let signatures = List.map (fun (a,b) -> List.map remove_kind a, List.map remove_kind b) signatures in
    let a, b = sign in
      (* Printf.printf "\n\n%s ---> %s\n\n" (String.concat "->" (List.map U.to_string a)) (String.concat "->" (List.map U.to_string b)); *)
     let subst_sig (a,b) =
      (* U.apply_all (subst_u subst) (List.map rename_v a), U.apply_all (subst_u subst) (List.map rename_v b) *)
      U.apply_all subst a, U.apply_all subst b
    in
    let sign = subst_sig sign in
    let a,b = sign in
    let sign = List.map remove_kind a, List.map remove_kind b in
    let sign = List.concat (List.map remove_kind_top a), List.concat (List.map remove_kind_top b)
    in
    let sign = normal_signature (ending_normalize sign) in
      sign
      
