{
module Token = struct
type t = Integer of int | Float of float | Word of string | String of string
end
}

let word = [^' ''\t''\n']+
let float_num = ['-''+']?['0'-'9']*'.'?['0'-'9']+(['e''E']['-''+']?['0'-'9']+)?
let whites = [' ''\t''\n']+
let endl = ['\n']
let any = _
rule next_token interpret accepting =
  parse 
    | whites     { next_token interpret accepting lexbuf }
    | float_num as t  { next_token interpret (interpret (Token.Float (float_of_string t))) lexbuf }
    | word as t  { next_token interpret (interpret (Token.Word t)) lexbuf }
{
  let next_block interpret = next_token interpret false (Lexing.from_channel stdin)
}
