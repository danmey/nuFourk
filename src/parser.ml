open BatParserCo
open BatCharParser
open BatString

let e = exactly
let white_chars = [' '; '\t'; '\n'; '\r';]
let is_white = flip List.mem **> white_chars
let is_not_white = not -| is_white

let whites = satisfy is_white
let to_string = implode |- return
let to_list = (fun x -> [x]) |- return
let null_list _ = []
let const x _ = x
let token = one_plus **> satisfy is_not_white >>= to_string
let spaces = one_plus **> satisfy is_white >>= (return -| const ())
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
  tryparse ((fun t -> if is_word t then t else failwith "Unknwon token!"), word) 
  **> tryparse (float_of_string, fnum) **> tryterm (int_of_string, inum)

let lex1 is_word = rev |- fold_left (fun acc t -> (parse_token is_word t) :: acc) []

let lex2 l = fold_right (fun a tree -> match a with Word "[" -> [Quote (rev tree)] | Word "]" -> tree | a -> tree@[a]) l []

let parse is_word str = lex2 (lex1 is_word (lex0 str))
