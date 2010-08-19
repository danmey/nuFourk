open Code
open Unify

open BatList

type signature = {
  input  : U.t list;
  output : U.t list;
}
    
type annotation = opcode * signature

let basic_type name = U.Term ( name, [ ] )

let t_int = basic_type "int"
let t_float = basic_type "float"

(*
let annotate { 
  input = input; 
  output = output;
} = 
  function
    | PushInt of int ->
      
    | PushFloat of float -> 
    | Call of string ->
    | PushCode of code ->
    | App ->
*)
  
