open BatParserCo
open BatCharParser
open BatString
open BatChar

let e = exactly
let white_chars = [' '; '\t'; '\n'; '\r';]
let is_not_white = not -| Char.is_whitespace

let whites = satisfy is_whitespace
let to_string = implode |- return
let to_list = (fun x -> [x]) |- return
let null_list _ = []
let const x _ = x
let token = one_plus **> satisfy is_not_white >>= to_string
let spaces = one_plus **> satisfy is_whitespace >>= (return -| const ())
let token_space = token >>= (fun t -> spaces <|> eof >>= (fun _ -> return t))
let lexer0 = zero_plus token_space
open BatStd

let lex0 str = let Ok a = run lexer0 **> source_of_string str in a

module Ast = struct
type t = FNum of float | INum of int | Word of string | Quote of t list
let inum = fun x -> INum x
let fnum = fun x -> FNum x
let word = fun x -> Word x
end

open List
open BatList

let tryparse (f,b) nxt = fun x -> try b (f x) with _ -> nxt x
let tryterm (f,b) = f |- b
let parse_token is_word = 
  tryparse ((fun t -> if is_word t then t else failwith "Unknwon token!"), Ast.word) 
  **> tryparse (float_of_string, Ast.fnum) **> tryterm (int_of_string, Ast.inum)

let lex1 is_word = rev |- fold_left (fun acc t -> (parse_token is_word t) :: acc) []

open Ast
let trav el (tree,depth) =  
  match el with
    | Word "[" -> [Quote tree], depth+1 
    | Word "]" -> tree, depth-1 
    | el -> el::tree, depth

let empty = [],0
let lex2 l = fold_right trav l empty

let parse is_word str = lex2 (lex1 is_word (lex0 str))

let rec merge (tree2,depth) =
  let td = (tree2,depth) in
  function
    | (Quote t),cur_depth when depth = cur_depth -> Quote (t@tree2), cur_depth+1
    | (Quote t),cur_depth -> (Quote (fst (merge_lst td t)), cur_depth+1)
    | a -> a 
and merge_lst td = 
  function
    | [] -> [],0
    | x::xs -> (fst **> merge td x)::(fst (merge_lst td xs)),0
  
  
