(*module S = struct*)

open List

let ($) f g = (fun a -> f (g a)) 
let (<|>) f g = f g

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

let  solve = fold_right (fun el c -> apply (unify el) c)
;;


(* insert x at all positions into l and return the list of results *)
let rec insert x l = 
  match l with
    | [] -> [[x]]
    | a::m -> (x::l)::(List.map (fun y -> a::y) (insert x m))
;;


(* list of all permutations of l *)
let rec perms l = 
  match l with
    | a::m -> List.flatten (List.map (insert a) (perms m))
    | _ -> [l]
;;

let rec remove l =
    match l with
      | [] -> []
      | x::xs -> xs::(map (fun z -> x::z) (remove xs))

let rec iter n f i = if n > 0 then iter (n-1) f (f i) else i

let rec unify = function
  | [] -> []
  | x::xs -> if exists ((=) x) xs then unify xs else x::(unify xs)
  
let rec combs n l = rev (unify (iter (length l-n) (fun a -> (flatten (map remove a))) [l]))

let valid_pair (c1,r1) (c2,r2) = (c1 = c2 && r1 = r2) || not(c1 = c2 || r1 = r2 || (c1-c2) - (r1-r2) = 0 || (c1-c2) + (r1-r2) = 0  )


let rec remove_but l =
    match l with
      | [] -> []
      | x::xs -> (x,xs)::(map (fun (e,z) -> e,(x::z)) (remove_but xs))

let rec loop cond trans state = if cond state then state else loop cond trans (trans state)

let rec insert x l = 
  match l with
    | [] -> [[x]]
    | a::m -> (x::l)::(List.map (fun y -> a::y) (insert x m))
;;


(* list of all permutations of l *)
let rec perms l = 
  match l with
    | a::m -> List.flatten (List.map (insert a) (perms m))
    | _ -> [l]
;;

let rec rem el = function
    | [] -> []
    | x::xs -> if el = 0 then rem (el-1) xs else x::(rem (el-1) xs)

let rec valid_board b = ignore(fold_left (fun c1 r1 -> ignore(fold_left (fun c2 r2 ->  if not (valid_pair (c1,r1) (c2,r2)) then raise Not_found else c2+1) 0 b); c1+1) 0 b)



(* list of all permutations of l *)
let rec solve l = 
  match l with
    | a::m -> fold_left (fun acc el -> try valid_board el; el::acc with Not_found->acc) [] (flatten (map (insert2 a) (perms m)))
    | _ -> [l]
;;

  
