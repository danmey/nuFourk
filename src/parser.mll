{
module Token = struct
type t = Integer of int | Float of float | Word of string | String of string
end
}

let word = [^' ''\t''\n']+
let float_num = ['-''+']?['0'-'9']+'.'['0'-'9']*(['e''E']['-''+']?['0'-'9']+)?
let int_num = ['-''+']?['0'-'9']+
let whites = [' ''\t''\n']+

rule next_token interpret state =
  parse 
    | whites          { next_token interpret state lexbuf }
    | float_num as t  { let state = interpret state (Token.Float (float_of_string t)) in next_token interpret state lexbuf }
    | int_num as t    { let state = interpret state (Token.Integer (int_of_string t)) in next_token interpret state lexbuf }
    | word as t       { let state = interpret state (Token.Word t) in next_token interpret state lexbuf }
{
  let next_block interpret state = next_token interpret state (Lexing.from_channel stdin)
}
