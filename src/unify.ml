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

(*module S = struct*)

open BatPervasives
open BatChar

open List

let ($) f g = (fun a -> f (g a)) 
let (<|>) f g = f g

module U = struct

type t =
  | Var of string
  | Term of string * t list

let rec vars = function
  | Var n' -> [n']
  | Term (_,l) -> flatten (map vars l)

let prim name = Term (name, [])

let var name = Var name

let arrow lhs rhs = Term ("arrow", [Term ("in", lhs); Term ("out", rhs)])

let kind lst = Term ("kind", lst)

let rec occurs t n = mem n (vars t)
  
let rec apply s = function
  | Var n -> (try assoc n s with Not_found -> Var n)
  | Term (n,l)-> Term (n, map (apply s) l)

let i = ignore  

let compose s2 s1 = 
  map (fun (v,t) -> v,(apply s1 t)) s2 @ 
    filter (fun (v,_) -> try i <|> assoc v s2; false with Not_found -> true) s1

exception Unify_fail of string * string

let rec rename i = function
  | Var n' -> Var (Printf.sprintf "%s%d" n' i) 
  | Term (n,l) -> Term (n, List.map (rename i) l)

 let rec combine_l x y =
    let rec loop = function
      | x :: xs,y :: ys -> let lst, rest = loop (xs,ys) in ((x,y) :: lst), rest
      | [], x -> [],x
      | x, [] -> [],x
    in
      loop (x,y)

let rec to_string = 
  function
    | Term (nm, l) ->
	Printf.sprintf "(%s: %s)" nm 
	  (String.concat " "  (List.map to_string l))
    | Var (nm) -> Printf.sprintf "%s'" nm

(* let rec pre_unify (a, b) =  *)
(*   match a,b with *)
(*     | Term(n1, Var(a)::l1), Term(n2, l2) when *)
(* 	is_uppercase (a.[0]) *)
(* 	-> *)
(* 	  let l1 = List.rev l1 in *)
(* 	  let l2 = List.rev l2 in *)
(* 	  let combined, rest = (combine_l l1 l2) in *)
(* 	  let combined = List.rev combined in *)
(* 	  let rest = List.rev rest in *)
(* 	  let combined = List.map pre_unify combined in *)
(* 	  let l1, l2 = List.split combined in *)
(* 	    Term(n1, Var(a)::l1), Term(n2, Term ("kind", rest)::l2) *)

(*     |  Term(n1, l1), Term(n2, Var(a)::l2) when *)
(* 	is_uppercase (a.[0]) *)
(* 	-> *)
(* 	  let l1 = List.rev l1 in *)
(* 	  let l2 = List.rev l2 in *)
(* 	  let combined, rest = (combine_l l1 l2) in *)
(* 	  let combined = List.rev combined in *)
(* 	  let rest = List.rev rest in *)
(* 	  let combined = List.map pre_unify combined in *)
(* 	  let l1, l2 = List.split combined in *)
(* 	    Term(n1, Term ("kind", rest)::l1), Term(n2, Var(a)::l2) *)

(*     | Term(n1,l1),Term(n2,l2) -> *)
(*       (\* Printf.printf "\n\npre_unify: %s --> %s\n\n" (to_string a) (to_string b); *\) *)
(*       Printf.printf "\n\npre_unify: %s ---> %s\n\n" (to_string a) (to_string b); *)
(*       let combined =  try combine l1 l2 with | _ -> raise (Unify_fail ("Awrong arity", "")) in *)
(*       let combined = List.map pre_unify combined in *)
(*       let l1, l2 = List.split combined in *)
(*     	Term(n1, l1), Term(n2, l2) *)

    (* | a, b -> a, b *)
    let rec remove_kind = function
      | Term (nm, [Term ("kind", lst)]) -> Term (nm, List.map remove_kind lst)
      | a -> a

let rec unify (a, b) = 
  (* let a,b = pre_unify (a,b) in *)
  print_endline "--------------------";
   Printf.printf "unify: %s ---> %s\n\n" (to_string a) (to_string b);
   let a,b = remove_kind a, remove_kind b in
  
   Printf.printf "unify: %s ---> %s\n\n" (to_string a) (to_string b);
   (* let a, b = match a, b with  *)
   (*   | Term ("kind", lst), a -> Term ("kind", lst), Term ("kind", [a]) *)
   (*   | a, Term ("kind", lst) -> Term ("kind", [a]), Term ("kind", lst) *)
   (*   | el -> el *)
   (* in *)
   (* Printf.printf "unify: %s ---> %s\n\n" (to_string a) (to_string b);  *)
  match a,b with
    | Term ("kind", ((a::lr) as l)), ((Term (n2, [])) as b)  ->
      unify (a, b) @ ["left", Term("kind", lr);"right", Term("kind", [Term(n2,[])])]

    |  ((Term (n2, [])) as a), Term("kind", ((b::lr) as l))  ->
      unify (a, b) @ ["right", Term("kind", lr); "left", Term("kind", [Term(n2,[])])]

    | Term ("in", a), ((Term ("in", [])) as b)  ->
      ["left", Term("kind", a);]

    | ((Term ("in", [])) as b),Term ("in", a)  ->
      ["right", Term("kind", a);]

    | Term(n1, Var(a)::l1), Term(n2, l2) when
	is_uppercase (a.[0])
	->
	  let l1 = List.rev l1 in
	  let l2 = List.rev l2 in
	  let combined, rest = (combine_l l1 l2) in
	  let combined = List.rev combined in
	  let rest = List.rev rest in
	  let result = List.concat (List.map unify combined) in
	  if rest <> [] then [a, Term ("kind", rest)] else [] @ fold_left 
	    (fun s (t1',t2') -> 
	      compose (unify (apply s t1', apply s t2')) s)
	    [] combined @ result

    |  Term(n1, l1), Term(n2, Var(a)::l2) when
	is_uppercase (a.[0])
	->
	  let l1 = List.rev l1 in
	  let l2 = List.rev l2 in
	  let combined, rest = (combine_l l1 l2) in
	  let combined = List.rev combined in
	  let rest = List.rev rest in
	  let result = List.concat (List.map unify combined) in
	  if rest <> [] then [a, Term ("kind", rest)] else [] @ 
            fold_left 
	    (fun s (t1',t2') -> 
	      compose (unify (apply s t1', apply s t2')) s)
	    [] combined @ result

    | Var(n), t -> 
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail (n,"OCCUR"))
	else [n,t]
    | t, Var(n) ->
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail ("OCCUR",n))
	else [n,t]

    | Term(n1, l1), Term(n2, l2) ->
      if n1 <> n2 then raise (Unify_fail (n1,n2))
      else
	let combined = try combine l1 l2 with | _ -> raise (Unify_fail ("Bwrong arity", "")) in
	  fold_left 
	    (fun s (t1',t2') -> 
	      compose (unify (apply s t1', apply s t2')) s)
	    []  combined
	

open List
let apply_all subs exps =
  fold_left (fun exp sub -> map (apply subs) exps) exps exps



end

