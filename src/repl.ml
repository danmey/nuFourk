open Lexer
open Engine
open BatPervasives

module Repl = struct
  open List
  let process model loop =
    loop |> next_block Run.run model
end

let rec loop model = 
  Repl.process model loop

;;
  
main();;
