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

let rec occurs t n = mem n (vars t)
  
let rec apply s = function
  | Var n -> (try assoc n s with Not_found -> Var n)
  | Term (n,l) -> Term (n, map (apply s) l)

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
      | x :: xs,y :: ys -> (x,y) :: loop (xs,ys)
      | [], _ -> []
      | _, [] -> []
    in
      loop (x,y)

let rec to_string = 
  function
    | Term (nm, l) when List.length l == 0 -> Printf.sprintf "%s" nm
    | Term (nm, l) ->
	Printf.sprintf "(%s %s)" nm **> String.concat " " **> List.map to_string l
    | Var (nm) -> Printf.sprintf "%s'" nm

let rec unify (a, b) = 
  (* Printf.printf "%s :: %s\n\n" (to_string a) (to_string b); *)
  match a,b with
  | Term("code", 
	 [Term("list",[]);e]), Term("code", [Term("list", [c]); Term ("list", [d])]) ->
    (match c,d with | _, d -> unify (d,e))
  | Var(n), t -> 
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail (n,"OCCUR"))
	else [n,t]
  | t, Var(n) ->
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail ("OCCUR",n))
	else [n,t]
  | Term(n1, t1), Term(n2, t2) ->
      if n1 <> n2 then raise (Unify_fail (n1,n2))
      else
	fold_left 
	  (fun s (t1',t2') -> 
	     compose (unify ((apply s t1'), apply s t2')) s)
	  [] (combine_l t1 t2)

open List
let apply_all subs exps =
  fold_left (fun exp sub -> map (apply subs) exps) exps exps



end

