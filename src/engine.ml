open BatLexing
open BatPervasives
open BatOption
open Unification

module Error = struct
  exception Runtime_Type of string
  exception Stack_Underflow
  exception Symbol_Not_Bound of string
  exception Parse_Error of string
end

module Code = struct
  type opcode = 
      PushInt of int 
    | PushFloat of float 
    | Call of string 
    | PushCode of t
    | App
  and t = opcode list

  let rec to_string = 
    function
      | PushInt v -> Printf.sprintf "%d " v
      | PushFloat v -> Printf.sprintf "%ff " v
      | Call nm -> Printf.sprintf "%s " nm
      | PushCode c -> Printf.sprintf "[ %s ]" **> String.concat " " **> List.map to_string c

  let compile = List.map **>
    function 
      | Lexer.Token.Integer v -> PushInt v 
      | Lexer.Token.Float v -> PushFloat v
      | Lexer.Token.Word v -> Call v
end

module Ref = struct
  type t = int
end

module Value = struct
  type t = Int of int | Float of float | Ref of Ref.t | Empty | Code of Code.opcode list
  let to_string = function
    | Int _ -> "int"
    | Float _ -> "float"
    | Ref _ -> "ref"
    | Empty -> "empty"
    | Code _ -> "code"
end

module Cell = struct
  type t = Value.t
end

module type Named = sig
  type t
  val name : t -> string
end

module type Symbol = sig
  type t
  type code
  val name : t -> string
  val contents : t -> code
  val call : t -> (unit -> unit)
end

module Word =
struct
  type kind = Macro | Compiled
  type code = Core of (unit -> unit) (* * Signature.t *) | User of Code.opcode list
  type t = { name:string; code:code; kind:kind; }
  let def name kind s code = { name = name; code = Core (code (*, s *)); kind = kind }
  let def_user name code = { name = name; code = User code; kind = Compiled }
  let empty name = { name = name; code = User []; kind = Compiled }
  let contents w = w.code
  let name w = w.name
  let is_core w = match w.code with Core _ -> true | _ -> false
  let call w = match w.code with Core f -> f | _ -> assert (false)
  let code w = match w.code with User l -> l | _ -> assert (false)
end

module Dictionary = struct
  module type S = sig
    module Entry : Symbol;;
    type t
    type symbol
    val create : unit -> t
    val add_symbol  : t -> symbol -> unit
    val lookup_symbol : t -> string -> symbol
  end
  module Make (Entry : Symbol) : S with type symbol = Entry.t = struct
    module Entry = Entry
    type symbol = Entry.t
    type t = (string, symbol) Hashtbl.t
    let create () = Hashtbl.create 1000
    let add_symbol d w = Hashtbl.add d (Entry.name w) w
    let lookup_symbol d n = Hashtbl.find d n
  end
end

module Signatures = struct
  module type S = sig
    module Dict : Dictionary.S;;
    type dict
    type symbol
    type t
    val of_symbol : dict -> symbol -> t
    val of_code   : dict -> Code.t -> t
    val signature : (string list * string list) -> t
  end
  module Make (Sym : Symbol) : S 
    with type symbol = Sym.t = struct
      module Dict = Dictionary.Make(Sym);;
      type dict = Dict.t
      type symbol =  Dict.symbol
      type t = { return: U.t list; arguments: U.t list; }
      open Stack
      open Code
      let st = List.map (fun x -> U.Term (x,[]))
      let signature (a,b) = { return = st a; arguments = st b }
      let to_list st = let ret = ref [] in iter (fun el -> ret := !ret@[el]) st; !ret
      let rec of_code model code = 
	let arguments = Stack.create() in
	let stack = Stack.create() in
	let empty st = try top st; false with Empty -> true in
	let expect typ = 
	  if empty stack then push typ arguments else
	    (let typ' = pop stack in
	       try
		 match U.unify (typ', typ) with
		   | [] ->  push typ' stack
		   | (_,a)::_ -> push a stack
	       with _ -> raise (Error.Runtime_Type (Printf.sprintf "Expected type `%s', found `%s'!" (U.to_string typ) (U.to_string typ'))))
    in
    let st nm = U.Term (nm, []) in
      List.iter 
  (function 
    | PushInt _ -> push (st "int") stack
    | PushFloat _ -> push (st "float") stack
    | PushCode c ->
      let { arguments=arguments; return=return } = of_code model c 
      in push (U.Term ("code", arguments)) stack
    | Call name -> 
      let w = Dict.lookup_symbol model name in
      let s = of_symbol model w in
	List.iter (fun typ -> expect typ) s.arguments;
	List.iter (fun typ -> push typ stack) **> List.rev s.return;
    | App -> 
      expect (U.Term ("code", [U.Var "a"]))
  ) code;
      { arguments = to_list arguments; return = to_list stack }
  and of_symbol model word = { arguments = []; return = [] }
(*      match Sym.contents word with
	| User code -> of_code model code
	| Core (nm,s) -> s
*)
  let print { return=return; arguments=arguments } =
    print_string "( ";
    (for i = 0 to List.length arguments-1 do
      print_string **> U.to_string **> List.nth arguments i;
      print_string " ";
     done;);
    print_string "-> ";
    for i = 0 to List.length return-1 do
      print_string **> U.to_string **> (List.nth return i);
      print_string " ";
    done;
    print_string ")";
    end
end

module Model = struct
  module type S = sig
    module Dict : Dictionary.S;;
    module Sym  : Symbol;;
    type state = Interpreting | Compiling
    type t
    val create        : int -> t
    val push_value    : t -> Value.t -> unit
    val pop_value     : t -> Value.t
    val append_opcode : t -> Code.opcode -> unit
    val stack : t -> Cell.t Stack.t
    val lookup_symbol : t -> string -> Dict.symbol
    val state         : t -> state
    val lexbuf        : t -> Lexing.lexbuf
  end
  module Make (Dict : Dictionary.S) : S = struct
    module Dict = Dict;;
    module Sym = Dict.Entry;;
    open Stack
    type state = Interpreting | Compiling
    type t = 
	{ stack  : Cell.t Stack.t; 
	  cells  : Cell.t array; 
	  dict   : Dict.t;
	  lexbuf : Lexing.lexbuf;
	  codebuf: Code.opcode list ref Stack.t;
	  mutable state: state;
	}
    let create heap_size = 
      let m = { 
	stack  = Stack.create(); 
	cells  = Array.create heap_size Value.Empty; 
	dict   = Dict.create();
	lexbuf = from_input stdin;
	codebuf = Stack.create();
	state = Interpreting
      } in
	m
    let lookup_symbol model name = try Dict.lookup_symbol model.dict name with | Not_found -> raise (Error.Symbol_Not_Bound ( "Symbol `" ^ name ^ "' is not found in this context!"))
    let append_opcode model token = let l = top model.codebuf in l := token::!l
    let stack model = model.stack
    let state w = w.state
    let lexbuf m = m.lexbuf
    let push_value m v = push v m.stack
    let pop_value m = pop m.stack
  end
end
module Run : sig
  module Sym : Symbol;;
  val next_token    : t -> (t -> Lexer.Token.t -> t) -> t
  val execute_word : M.t ->  Sym.t -> unit
  val execute_code : Model.S.t -> Code.opcode list -> unit
  val execute_symbol : Model.t -> string -> unit
  val run : Model.t -> Lexer.Token.t -> Model.t
  val expect : Model.t -> (Model.t -> Lexer.Token.t -> Model.t) -> Model.t
  val continue: Model.t -> Model.t
  val start : unit -> Model.t
end = struct
  module Model = Model.Make(Dictionary.Make(Word));;
  module Sym = Model.Sym;;
  open Lexer
  open Model
  open Stack
  let push_int model i = push (Value.Int i) **> stack model
  let push_float model f = push (Value.Float f) **> stack model
  
  let push_code model f = push (Value.Code f) **> stack model
  let rec execute_word model word =
    if Word.is_core word then Sym.call word()
	else execute_code model (Sym.code word)
  and execute_code model =
    List.iter (function
      | Code.PushInt v -> push_int model v
      | Code.PushFloat v -> push_float model v
      | Code.PushCode v -> execute_code model v
      | Code.Call w -> execute_symbol model w)
  and execute_symbol model symbol = 
    let w = Model.lookup_symbol model symbol in
      execute_word model w
  let pop_int model =
    try
      match pop **> stack model with 
	| Value.Int i -> i
	| a -> raise (Error.Runtime_Type("Expected type `int' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow
  
  let run model token =
    let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout in
      (try
	 match state model with
	  | Model.Interpreting -> 
	    (match token with
	      | Token.Integer value -> push_int model value;()
	      | Token.Float value -> push_float model value;()
	      | Token.Word name -> execute_symbol model name)
	  | Model.Compiling -> 
	    (match token with
	      | Token.Integer v -> append_opcode model **> Code.PushInt v
	      | Token.Float v -> append_opcode model **> Code.PushFloat v
	      | Token.Word "!" -> append_opcode model **> Code.App
	      | Token.Word name -> 
		let w = lookup_symbol model name in
		  (match w.Word.kind with 
		    | Word.Macro -> ignore(execute_symbol model name)
		    | Word.Compiled -> append_opcode model **> Code.Call name))
      with
	| Error.Runtime_Type str -> top_er str
	| Error.Symbol_Not_Bound str -> top_er str
	| Error.Stack_Underflow -> top_er "Stack underflow!");
      model
  let expect model kont = Lexer.next_token kont model **> lexbuf model
  let continue model = expect model run
  let next_token model kont = expect model (fun model token -> let m = kont model token in Run.continue model)
    
  
  let pop_code model =
    try
      match pop **> stack model with 
	| Value.Code i -> i
	| a -> raise (Error.Runtime_Type("Expected type `code' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow
  
  let pop_float model =
    try
      match pop **> stack model with 
	| Value.Float i -> i
	| a -> raise (Error.Runtime_Type("Expected type `float' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

	
	
end
let start() = 
  let model = Model.create 1000 in
    Boostrap.init model;
    continue model

module Boostrap : sig
  val init : Model.t -> Model.t
end = struct
  module Model = Model.Make(Word);;
  open Stack
  open Model
     let push_int model i = push (Value.Int i) **> stack model
    
  let pop_int model =
    try
      match pop **> stack model with 
	| Value.Int i -> i
	| a -> raise (Error.Runtime_Type("Expected type `int' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow
  
  let push_float model f = push (Value.Float f) **> stack model
  
  let push_code model f = push (Value.Code f) **> stack model
  
  let pop_code model =
    try
      match pop **> stack model with 
	| Value.Code i -> i
	| a -> raise (Error.Runtime_Type("Expected type `code' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow
  
  let pop_float model =
    try
      match pop **> stack model with 
	| Value.Float i -> i
	| a -> raise (Error.Runtime_Type("Expected type `float' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow
 
  open Model
  open Word
  module Types = Signatures.Make(Word);;
  module Dictionary = Dictionary.Make(Word);;
  let add_symbol model word = Dictionary.add_symbol model.dict word ; model

  let swap (a,b) = (b,a)
  let init model = 

    let app_pair f model = 
      swap (f model, f model) 
    in

    let app2 op pushv pop model = 
      let (a,b) = app_pair pop model in
	op a b |> pushv model
    in

    let app2i op = app2 op push_int pop_int in

    let app2f op = app2 op push_float pop_float in

    let lift1 f pop model = 
      let a = pop model in 
	f a
    in
    let lift1i f model = lift1 f pop_int model in
    let lift1f f model = lift1 f pop_float model in
    let lift1c f model = lift1 f pop_code model in

    let tok f model = Model.next_token model
      (fun model -> function
	| Lexer.Token.Word w -> f w; model
	| _ -> raise (Error.Parse_Error "Expected token `name' not token `value'")
      ); () 
    in
    let tok1 f model = Model.next_token model 
      (fun model -> function
	| Lexer.Token.Word w -> f model w; model
	| _ -> raise (Error.Parse_Error "Expected token `name' not token `value'")
      ); () 
    in
    let with_flush f a = f a; flush stdout in
    let st = List.map (fun x -> U.Term (x,[]))
    in
      [
	
      def "+" Compiled (Types.signature (["int";"int"], ["int"])) **> app2i ( + );
      def "f+" Compiled (Types.signature (["float";"float"], ["float"]))  **> app2f ( +. );

(*      def "-" Compiled **> app2 ( - );
      def "*" Compiled **> app2 ( * );
      def "/" Compiled **> app2 ( / ); 
*)
(*
      def "." Compiled { Types.arguments = st ["int"]; Types.return = [] } **> lift1i **> with_flush print_int;
      def "f." Compiled { Types.arguments = st ["float"]; Types.return = [] } **> lift1f **> with_flush print_float;
      def "[" Macro { Types.arguments = []; Types.return = [] }**> (fun model -> Stack.push (ref []) model.codebuf; model.state <- Compiling);
      def "]" Macro { Types.arguments = []; Types.return = [U.Term ("code", [U.Var "a";U.Var "b"])] }    **> (fun model -> 
	let code = !(Stack.pop model.codebuf) in
	  Types.signature_of_code model **> List.rev **> code;
	  (if Stack.is_empty model.codebuf then (model.state <- Interpreting; push_code model) else
	      (fun l -> append_opcode model **> Code.PushCode l)) **> List.rev code);
      
      def ".." Compiled { Types.arguments = st ["code"]; Types.return = [] }   **> (fun model -> 
	print_string "[ "; 
	List.iter (fun x -> Printf.printf "%s " **> Code.to_string x) **> List.rev **> pop_code model; 
	print_string "]"; 
	flush stdout);
      def ":" Macro { Types.arguments = []; Types.return = [] } **> 
	tok1 **> 
	(fun model name -> 
	  let code = pop_code model in
	  Types.signature_of_code model code;
	  Dictionary.add model.dict **> Word.def_user name **> code;
	);
      def "!" Compiled { Types.arguments = [U.Term ("code", [U.Var "a";U.Var "b"])]; Types.return = [U.Var "b"] } **> lift1c **>
	(fun code -> 
	  Run.execute_code model code
	);

      def "check" Compiled { Types.arguments = [U.Term ("code", [U.Var "a";U.Var "b"])]; Types.return = [] } **> lift1c **>
	(fun code -> 
	  Types.print **> Types.signature_of_code model code;
	  flush stdout;
	);
      def "type" Macro { Types.arguments = []; Types.return = [] } **> tok **> with_flush 
	(fun name ->
	  let word = lookup_symbol model name in
	  let s = Types.signature_of_word model word in 
	    Types.print s; print_endline ""
	)*)
      ] |> List.fold_left add_symbol model

(*
    let run (model,code) token = 
      let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout; model in
	try
	  (match token with
	    | Token.Integer value -> push_int model value
	    | Token.Word name -> perform_symbol model name);
	  model
	with
	  | Error.Runtime_Type str -> top_er str
	  | Error.Symbol_Not_Bound str -> top_er str
	  | Error.Stack_Underflow -> top_er "Stack underflow!"
      
  let rec loop_until model f p =
    let model = Lexer.next_token f (model,[]) model.lexbuf in
      loop_until model f

    let comp
  let create model name =
  add_symbol model empty
*)

end

let main () =  Run.start()

