open BatPervasives

module State = struct
  type t = Interpreting | Compiling
end

module Handle = struct
  type t = int
end

module Name = struct
  type t = string
end

module rec Opcode : sig 
  type t = PushInt of int | PushFloat of float | Call of Name.t | Code of Code.t
  val to_string : t -> string
end = struct
  type t = PushInt of int | PushFloat of float | Call of Name.t | Code of Code.t
  let to_string = function
    | PushInt v -> Printf.sprintf "d:%d " v
    | PushFloat v -> Printf.sprintf "f:%f " v
    | Call nm -> Printf.sprintf "%s " nm

end
and Code : sig type t end = struct
  type t = Opcode.t list
end

module Ref = struct
  type t = int
end

module Stack = struct
  type 'a t = 'a list
  let push value stack = value::stack
  let pop (value::stack) = value,stack
  let top (value::stack) = value
  let empty = function [] -> true | _ -> false
  let create() = []
end

module rec Word : sig 
  type kind = Immediate | Compiled
  type code = Core of (Model.t -> Model.t) | User of Opcode.t list
  type t = { name:Name.t; code:code; kind:kind; }
  val def : Name.t -> kind -> (Model.t -> Model.t) -> t
end = struct
  type kind = Immediate | Compiled

  type code = Core of (Model.t -> Model.t) | User of Opcode.t list

  type t = { name:Name.t; code:code; kind:kind; }

  let def name kind code = { name = name; code = Core code; kind = kind }
end and Quotation : sig 
  type t = Opcode.t list
  val to_string : t -> string
  val make : unit -> Opcode.t list 
end = struct 
  type t = Opcode.t list 
  let to_string q = String.concat " " **> List.map Opcode.to_string q
  let make() = [] 
end and Value : sig 
  type t = Int of int | Float of int | Ref of Ref.t | Empty | Quotation of Quotation.t 
end = struct
  type t = Int of int | Float of int | Ref of Ref.t | Empty | Quotation of Quotation.t
end
and Cell : sig type t = Value.t end = struct
  type t = Value.t
end and Dictionary : sig 
  type t = (Name.t, Word.t) Hashtbl.t
  val lookup : t -> Name.t -> Word.t
  val add    : t -> Word.t -> unit
  val create : unit -> t 
end = struct
  type t = (Name.t, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  open Hashtbl
  open Word
  let lookup = find
  let add dict word = add dict word.name word
end
and Model : sig 
  type t = 
      { stack  : Cell.t Stack.t; 
	qstack : Quotation.t Stack.t;
	cells  : Cell.t array; 
	dict   : Dictionary.t; 
      } 
  val create     : int -> t
  val push_value : t -> Value.t -> t
  val pop_value  : t -> t * Value.t
  val push_int_value : t -> int -> t
  val pop_int_value  : t -> t * int
  val add : t -> Word.t -> t
  val lookup : t -> Name.t -> Word.t
  val begin_quote : t -> t
  val end_quote : t -> t
  val state : t -> State.t
  val nested_state : t -> State.t
  val current_quote : t -> Quotation.t
  val compile : t -> Opcode.t -> t
end = struct
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
  open Stack
  let push_value model v = { model with stack=push v model.stack }
  let pop_value model = let v, s = pop model.stack in { model with stack=s },v
  let push_int_value model v = { model with stack=push (Value.Int v) model.stack }
  let pop_int_value model = let Value.Int v, s = pop model.stack in { model with stack=s },v
  let add model word = Dictionary.add model.dict word ; model
  let lookup model = Dictionary.lookup model.dict
  let begin_quote model = { model with qstack=push (Quotation.make()) model.qstack }
  let end_quote model = 
    let v,st = pop model.qstack in 
      push_value { model with qstack=st } (Value.Quotation v)
  let state model = if empty model.qstack then State.Interpreting else State.Compiling
  let nested_state model = if empty (snd **> pop model.qstack) then State.Interpreting else State.Compiling
  let current_quote model = top model.qstack
  let compile model name = let q,s = pop model.qstack in { model with qstack=push (q@[name]) s}
end

module Boostrap = struct
  open Model
  open Word

  let init model = 
    let pop2 model = 

      let model, a = pop_int_value model in 
      let model, b = pop_int_value model in 
	(b,a),model in

    let app2 op model = 
      let (a,b), model = pop2 model in
	op a b |> push_int_value model 
    in

    let eat1 f model =
      let model, a = pop_int_value model in 
	f a; model
    in

    let eata1 f model =
      let model, a = pop_value model in 
	f a; model
    in
      
    let with_flush f a = f a; flush stdout
    in
    [

      def "+" Compiled **> app2 (+);
      def "-" Compiled **> app2 (-);
      def "*" Compiled **> app2 ( *);
      def "/" Compiled **> app2 (/);
      def "." Compiled **> eat1 **> with_flush print_int;
      def "q." Compiled **> eata1 **> with_flush (fun (Value.Quotation q) -> print_endline (Quotation.to_string q););
      def "[" Immediate **> begin_quote;
      def "]" Immediate **> end_quote;
    ] |> List.fold_left add model
end

module Run = struct
  open Parser
  open State
  open Model
  open Word


  let run model token = 
    let perform word state model = 
      let f = match word.code with Core f -> f in
	match state,word.kind with 
	  | Interpreting, _  -> f model
	  | Compiling, Immediate -> 
	    (match nested_state model with
	      | Compiling -> compile model (Opcode.Call word.name)
	      | Interpreting -> f model)
	  | Compiling, Compiled -> compile model (Opcode.Call word.name)
    in
    match state model with
      | Interpreting -> 
	(match token with
	  | Token.Integer value -> push_value model (Value.Int value)
	  | Token.Word name -> 
	    let word = lookup model name in
	      perform word (state model) model)
      | Compiling    -> 
	(match token with
	  | Token.Integer value -> compile model (Opcode.PushInt value)
	  | Token.Word name -> 
	    let word = lookup model name in
	      perform word (state model) model)
end

let init() = 
  let model = Model.create 1000 in
    Boostrap.init model
