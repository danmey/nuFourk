open BatPervasives

type opcode = 
  | PushInt of int 
  | PushFloat of float 
  | Call of string 
  | PushCode of code
  | App
and code = opcode list
      
let rec to_string = 
  function
    | PushInt v -> Printf.sprintf "%d " v
    | PushFloat v -> Printf.sprintf "%ff " v
    | Call nm -> Printf.sprintf "%s " nm
    | PushCode c -> Printf.sprintf "[ %s ]" **> String.concat " " **> List.map to_string c

let compile = 
  List.map **>
    function 
      | Lexer.Token.Integer v -> PushInt v 
      | Lexer.Token.Float v -> PushFloat v
      | Lexer.Token.Word v -> Call v
