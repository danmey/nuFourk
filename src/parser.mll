{
module Token = struct
type t = Integer of int | Float of float | Word of string | String of string
end
}

let word = [^' ''\t''\n']+
let whites = [' ''\t''\n']+
let endl = ['\n']
let any = _
rule next_token interpret accepting =
  parse 
    | whites     { next_token interpret accepting lexbuf }
    | word as w  { next_token interpret (interpret (Token.Word w)) lexbuf }
{
  let next_block interpret = next_token interpret false (Lexing.from_channel stdin)
}
