open BatLexing
open BatPervasives
open BatOption
module Error = struct
  exception Runtime_Type of string
  exception Stack_Underflow
  exception Symbol_Not_Bound of string
  exception Parse_Error of string
end
module Name = struct
  type t = string
end

module Code = struct
  type opcode = PushInt of int | PushFloat of float | Call of Name.t

  let rec to_string = function
    | PushInt v -> Printf.sprintf "d:%d " v
    | PushFloat v -> Printf.sprintf "f:%f " v
    | Call nm -> Printf.sprintf "%s " nm
  let compile = List.map 
    (function 
      | Lexer.Token.Integer v -> PushInt v 
      | Lexer.Token.Float v -> PushFloat v
      | Lexer.Token.Word v -> Call v)
end

module Ref = struct
  type t = int
end

module Value = struct
  type t = Int of int | Float of float | Ref of Ref.t | Empty
  let to_string = function
    | Int _ -> "int"
    | Float _ -> "float"
    | Ref _ -> "ref"
    | Empty -> "empty"
end

module Cell = struct
  type t = Value.t
end

module rec Dictionary : sig 
  type t
  val lookup : t -> Name.t -> Word.t
  val add    : t -> Word.t -> unit
  val create : unit -> t 
end = struct
  open Hashtbl
  type t = (Name.t, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  let lookup = find
  let add dict word = add dict word.Word.name word
end
and Word : sig
    type kind = Macro | Compiled
    type code = Core of (Model.t -> unit) * Types.t | User of Code.opcode list
    type t = { name:Name.t; code:code; kind:kind; }
    val def : Name.t -> kind -> Types.t -> (Model.t -> unit) -> t
    val def_user : Name.t -> Code.opcode list -> t
end = struct
    type kind = Macro | Compiled
    type code = Core of (Model.t -> unit) * Types.t | User of Code.opcode list
    type t = { name:Name.t; code:code; kind:kind; }
    let def name kind s code = { name = name; code = Core (code,s); kind = kind }
    let def_user name code = { name = name; code = User code; kind = Compiled }
    let empty name = { name = name; code = User []; kind = Compiled }
end
and Types : sig
  type t = { return: string list; arguments: string list; }
  val signature_of_word : Model.t -> Word.t -> t
  val print : t -> unit
end = struct
  open Word
  open Model
  type t = { return: string list; arguments: string list; }
  open Stack
  open Code
  let rec signature_of_word model word = 
    let arguments = Stack.create() in
    let stack = Stack.create() in
    let empty st = try top st; false with Empty -> true in
    let expect typ = 
      if empty stack then push typ arguments else
	(let typ' = top stack in
	  if typ' = typ then
	    (pop stack;())
	  else
	    raise (Error.Runtime_Type (Printf.sprintf "Expected type `%s', found `%s'!" typ typ')))
    in
    let  to_list st = let ret = ref [] in iter (fun el -> ret := el::!ret) st; !ret in
      match word.Word.code with
      | User code -> 
	List.iter 
	  (function 
	    | PushInt _ -> push "int" stack
	    | PushFloat _ -> push "float" stack
	    | Call name -> 
	      let w = lookup_symbol model name in
	      let s = signature_of_word model w in
		List.iter (fun typ -> expect typ) s.arguments) code;
	{ arguments = to_list arguments; return = to_list stack }
      | Core (_,s) -> s


  let print { return=return; arguments=arguments } =
    print_string "( ";
    (for i = 0 to List.length arguments-1 do
      print_string (List.nth arguments i);
      print_string " ";
     done;);
    print_string " : ";
    for i = 0 to List.length return-1 do
      print_string (List.nth return i);
      print_string " ";
    done;
    print_string ")";
    
	
end
and Model : sig 
  type state = Interpreting | Compiling
  type t = 
      { stack  : Cell.t Stack.t; 
	cells  : Cell.t array; 
	dict   : Dictionary.t;
	lexbuf : Lexing.lexbuf;
	mutable tokenbuf: Lexer.Token.t list;
	mutable state: state;
      }
  val create         : int -> t
  val push_int       : t -> int -> unit
  val pop_int        : t -> int
  val push_float     : t -> float -> unit
  val pop_float      : t -> float
  val add_symbol     : t -> Word.t -> t
  val lookup_symbol  : t -> Name.t -> Word.t
  val next_token     : t -> (t -> Lexer.Token.t -> t) -> t
end = struct
  open Stack
  type state = Interpreting | Compiling
  type t = 
      { stack  : Cell.t Stack.t; 
	cells  : Cell.t array; 
	dict   : Dictionary.t;
	lexbuf : Lexing.lexbuf;
	mutable tokenbuf: Lexer.Token.t list;
	mutable state: state;
      }
  let create heap_size = 
    let m = { 
      stack  = Stack.create(); 
      cells  = Array.create heap_size Value.Empty; 
      dict   = Dictionary.create();
      lexbuf = from_input stdin;
      tokenbuf = [];
      state = Interpreting
    } in
      m

  let push_int model i = push (Value.Int i) model.stack
  let pop_int model =
    try
      match pop model.stack with 
	| Value.Int i -> i
	| a -> raise (Error.Runtime_Type("Expected type `int' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

  let push_float model f = push (Value.Float f) model.stack
  let pop_float model =
    try
      match pop model.stack with 
	| Value.Float i -> i
	| a -> raise (Error.Runtime_Type("Expected type `float' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

  let add_symbol model word = Dictionary.add model.dict word ; model
  let lookup_symbol model name = try Dictionary.lookup model.dict name with | Not_found -> raise (Error.Symbol_Not_Bound ( "Symbol `" ^ name ^ "' is not found in this context!"))
  let next_token model kont = Run.expect model (fun model token -> let m = kont model token in Run.continue model)
  let flush_code model = model.tokenbuf <- []
end
and Run : sig
  val run : Model.t -> Lexer.Token.t -> Model.t
  val expect : Model.t -> (Model.t -> Lexer.Token.t -> Model.t) -> Model.t
  val continue: Model.t -> Model.t
  val start : unit -> Model.t
end = struct
  open Lexer
  open Model
  open Word

  let run model token =
    let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout in
    let rec ex symbol =
	let w = lookup_symbol model symbol in
	  (match w.Word.code with
	    | Word.Core (f,_) -> f model;()
	    | Word.User code -> List.iter (function
		| Code.PushInt v -> push_int model v
		| Code.PushFloat v -> push_float model v
		| Code.Call w -> ex w) code
	  )
    in
      (try
	match model.state with
	  | Interpreting -> 
	    (match token with
	      | Token.Integer value -> push_int model value;()
	      | Token.Float value -> push_float model value;()
	      | Token.Word name -> ex name)
	  | Compiling -> 
	    (match token with
	      | Token.Integer value -> model.tokenbuf <- token::model.tokenbuf
	      | Token.Float value -> model.tokenbuf <- token::model.tokenbuf
	      | Token.Word name -> 
		let w = lookup_symbol model name in
		  (match w.Word.kind with 
		    | Word.Macro -> ignore(ex name)
		    | Word.Compiled -> model.tokenbuf <- token::model.tokenbuf))
      with
	| Error.Runtime_Type str -> top_er str
	| Error.Symbol_Not_Bound str -> top_er str
	| Error.Stack_Underflow -> top_er "Stack underflow!");
      model
	
  let expect model kont = Lexer.next_token kont model model.lexbuf
  let continue model = expect model run

let start() = 
  let model = Model.create 1000 in
    Boostrap.init model;
    continue model

end
and Boostrap : sig
  val init : Model.t -> Model.t
end = struct
    
  open Model
  open Word

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
    let with_flush f a = f a; flush stdout
    in
    [
      def "+" Compiled { Types.arguments = ["int";"int"]; Types.return = ["int"] }  **> app2i ( + );
      def "f+" Compiled { Types.arguments = ["float";"float"]; Types.return = ["float"] }    **> app2f ( +. );

(*      def "-" Compiled **> app2 ( - );
      def "*" Compiled **> app2 ( * );
      def "/" Compiled **> app2 ( / ); 
*)
      def "." Compiled { Types.arguments = ["int"]; Types.return = [] } **> lift1i **> with_flush print_int;
      def "f." Compiled { Types.arguments = ["float"]; Types.return = [] } **> lift1f **> with_flush print_float;
      def "[" Compiled { Types.arguments = []; Types.return = [] }**> (fun model -> model.state <- Compiling);
      def "]" Macro { Types.arguments = []; Types.return = [] }    **> (fun model -> model.state <- Interpreting);
      def ".." Macro { Types.arguments = []; Types.return = [] }   **> (fun model -> 
	print_string "[ "; 
	List.iter (fun x -> Printf.printf "%s " **> Lexer.Token.to_string x) **> List.rev model.tokenbuf; 
	print_string "]"; 
	flush stdout);
      def ":" Compiled { Types.arguments = []; Types.return = [] } **> tok1 **> (fun model name -> Dictionary.add model.dict **> Word.def_user name **> Code.compile **> List.rev model.tokenbuf; model.tokenbuf <- []);
      def "type" Compiled { Types.arguments = []; Types.return = [] } **> tok **> with_flush 
	(fun name ->
	  let word = lookup_symbol model name in
	  let s = Types.signature_of_word model word in 
	    Types.print s; print_endline ""
	)
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

