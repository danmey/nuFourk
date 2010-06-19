open Parser

module Repl = struct
  open List
  let interpret code = 
    print_endline "Interpreting:";
    print_endline (match code with Token.Word nm -> nm);
    flush stdout;
    true
  let process loop =
    next_block interpret;
    loop()
end

let rec main() = Repl.process main;;
  
main();;
