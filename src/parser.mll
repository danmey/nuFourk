{
module Token = struct
type t = Integer of int | Float of float | Word of string
end
}

let word = [^' ''\t''\n']+
let whites = [' ''\t']+
let endl = ['\n']
let topl_begin = ':'
let topl_end = ";"
let any = _
rule next_token interpret code in_topl =
  parse 
    | endl       { if not in_topl && code != [] then interpret (List.rev code) else next_token interpret code in_topl lexbuf }
    | whites     { next_token interpret code in_topl lexbuf }
    | topl_begin { next_token interpret code true lexbuf }
    | topl_end   { if in_topl then interpret (List.rev code) else failwith "Not in toplevel definition!" }
    | word as w  { next_token interpret ((Lexing.lexeme lexbuf)::code) in_topl lexbuf }

{
  let next_block interpret = next_token interpret [] false (Lexing.from_channel stdin)
}
