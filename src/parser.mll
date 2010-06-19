{
module Token = struct
type t = Integer of int | Float of float | Word of string
end
}

let word = "[^ \t]"
let whites = "[ \t]"
let endl = "\n"
let topl_begin = ":"
let topl_end = ";"
rule next_token code in_topl =
parse topl_begin { next_token [] true }
    | topl_end   { if in_topl then List.rev code,true else error "Not in toplevel definition!" }
    | endl       { if in_topl then next_token code in_topl else List.rev code,false }
    | word as w  { w::(next_token code in_topl) }
    | whites     { (next_token code in_topl) }
