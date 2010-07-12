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
let many_tokens = zero_plus token_space

let parse = run token -| source_of_string 

