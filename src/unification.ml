(*module S = struct*)

open BatPervasives

open List

let ($) f g = (fun a -> f (g a)) 
let (<|>) f g = f g

module U = struct
type t = Var of string 
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

exception Unify_fail

let rec unify = function
  | Var(n), t -> 
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail)
	else [n,t]
  | t, Var(n) ->
      if Var(n) = t then [] else
	if occurs t n then raise (Unify_fail)
	else [n,t]
  | Term(n1, t1), Term(n2, t2) ->
      if n1 <> n2 then raise Unify_fail
      else
	fold_left 
	  (fun s (t1',t2') -> 
	     compose (unify ((apply s t1'), apply s t2')) s)
	  [] (combine t1 t2)

let rec to_string = 
  function
    | Term (nm, l) when List.length l == 0 -> Printf.sprintf "%s" nm
    | Term (nm, l) ->
      (if List.length l == 1 then 
	Printf.sprintf "%s %s" nm
      else
	Printf.sprintf "(%s %s)" nm) **> String.concat " " **> List.map to_string l
    | Var (nm) -> Printf.sprintf "%s" nm

end
