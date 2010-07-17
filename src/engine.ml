open BatPervasives

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
  type opcode = PushInt of int | PushFloat of float | Call of Name.t | Code of t and t = opcode list

  let rec to_string = function
    | PushInt v -> Printf.sprintf "d:%d " v
    | PushFloat v -> Printf.sprintf "f:%f " v
    | Call nm -> Printf.sprintf "%s " nm
end

module Ref = struct
  type t = int
end

module Quotation = struct
  type t = Code.t
  let to_string q = String.concat " " **> List.map Code.to_string q
  let make() = [] 
end

module Value = struct
  type t = Int of int | Float of int | Ref of Ref.t | Empty | Quotation of Quotation.t
  let to_string = function
    | Int _ -> "int"
    | Float _ -> "float"
    | Ref _ -> "ref"
    | Empty -> "empty"
    | Quotation _ -> "code"
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
  open Word
  type t = (Name.t, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  let lookup = find
  let add dict word = add dict word.name word
end
and Word : sig
    type kind = Macro | Compiled
    type code = Core of (Model.t -> unit) | User of Code.t
    type t = { name:Name.t; code:code; kind:kind; }
    val def : Name.t -> kind -> (Model.t -> unit) -> t
end = struct
    type kind = Macro | Compiled
    type code = Core of (Model.t -> unit) | User of Code.t
    type t = { name:Name.t; code:code; kind:kind; }
    let def name kind code = { name = name; code = Core code; kind = kind }
end
and Model : sig 
  type t
  val create        : int -> t
  val push_int      : t -> int -> unit
  val pop_int       : t -> int
  val add_symbol    : t -> Word.t -> t
  val lookup_symbol : t -> Name.t -> Word.t
  val perform_symbol : t -> Name.t -> unit
end = struct
  open Stack
  type t = 
      { stack  : Cell.t Stack.t; 
	qstack : Quotation.t Stack.t;
	cells  : Cell.t array; 
	dict   : Dictionary.t; 
      }
  let create heap_size = { 
    stack  = Stack.create(); 
    qstack = Stack.create();
    cells  = Array.create heap_size Value.Empty; 
    dict   = Dictionary.create();
  }
  let push_int model i = push (Value.Int i) model.stack
  let pop_int model = 
    try
      match pop model.stack with 
	| Value.Int i -> i
	| a -> raise (Error.Runtime_Type("Expected type `int' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

  let add_symbol model word = Dictionary.add model.dict word ; model
  let lookup_symbol model = Dictionary.lookup model.dict
  let perform_symbol model symbol = 
    try 
      match (lookup_symbol model symbol).Word.code with
	| Word.Core f -> f model
    with Not_found -> raise (Error.Symbol_Not_Bound ( "Symbol `" ^ symbol ^ "' is not found in this context!"))
end

module rec Run : sig
  val run : Model.t -> Lexer.Token.t -> Model.t
  val process : Model.t -> (Model.t -> Lexer.Token.t -> Model.t) -> Model.t
  val loop : Model.t -> Model.t
  val init : unit -> Model.t
end = struct
  open Lexer
  open Model
  open Word


  let run model token = 
    (match token with
      | Token.Integer value -> push_int model value
      | Token.Word name -> perform_symbol model name);
    model

  let process model loop =
      loop |> Lexer.next_block Run.run model

let rec loop model = 
  process model loop


let init() = 
  let model = Model.create 1000 in
    Boostrap.init model

end
and Boostrap : sig
  val init : Model.t -> Model.t
end = struct
    
  open Model
  open Word

  let swap = uncurry **> flip **> curry identity
  let init model = 
    let pop2 model = 
      swap (pop_int model, pop_int model) 
    in
    let app2 op model = 
      let (a,b) = pop2 model in
	op a b |> push_int model 
    in

    let lift1 f model =
      let a = pop_int model in 
	f a
    in

    let with_flush f a = f a; flush stdout
    in
    [
      def "+" Compiled **> app2 (+);
      def "-" Compiled **> app2 (-);
      def "*" Compiled **> app2 ( * );
      def "/" Compiled **> app2 (/);
      def "." Compiled **> lift1 **> with_flush print_int;
      def "check" Compiled **> (fun model -> Lexer.next_block (fun _ -> 
	function 
	  | Lexer.Token.Word w -> print_endline w; flush stdout; model; Run.process model Run.run
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'")
      ) model)
    ] |> List.fold_left add_symbol model
end

let main () =
  let model = Run.init() in
    Run.loop model;
