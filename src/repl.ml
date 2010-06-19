open Parser

module Repl = struct
  open List
  let compile code = 
    print_endline "Compiling:";
    iter print_endline code;
    flush stdout;
    ()
  let interpret code = 
    print_endline "Interpreting:";
    iter print_endline code;
    flush stdout;
    ()
  let process loop =
    next_block interpret;
    loop()
end

let rec main() = Repl.process main;;
  
main();;
