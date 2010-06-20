module State = struct
  type t = Interpreting | Compiling
end

module Handle = struct
  type t = int
end

module Name = struct
  type t = string
end

module rec Opcode : sig type t end = struct
  type t = PushInt of int | PushFloat of float | PushLabel of Code.t | Call of Name.t
end
and Code : sig type t end = struct
  type t = Opcode.t list
end

module Ref = struct
  type t = int
end

module Value = struct
  type t = Int of int | Float of int | Ref of Ref.t | Empty
end

module Cell = struct
  type t = Value.t
end

(*
module Stackop = struct
  let push = Stack.push
  let pop = Stack.pop
  let swap stack = 
    let a = pop stack in
    let b = pop stack in
      push a stack; push b stack
  let rot stack =
    let a = pop stack in
    let b = pop stack in
    let c = pop stack in
      push b stack; push c stack; push stack a
end
*)

module Stack = struct
  type 'a t = 'a list
  let push value stack = value::stack
  let pop (value::stack) = value,stack
  let create() = []
end
module rec Word : sig type t end = struct
  type kind = Immediate | Compiled
  type code = Core of (Model.t -> Model.t) | User of Opcode.t list
  type t = { name:Name.t; code:code; kind:kind; }
  let call word = word
end
and Dictionary : sig 
  type t 
  val lookup : t -> Name.t -> Word.t
	 val add    : t -> Name.t -> Word.t -> unit 
	 val create : unit -> t 
end = struct
  type t = (Name.t, Word.t) Hashtbl.t
  let create() = Hashtbl.create 1000
  open Hashtbl
  let lookup = find  
  let add = add	
end
and Model : sig 
  type t = 
      { istack:int Stack.t; 
	fstack:float Stack.t; 
	cells:Cell.t array; 
	dict:Dictionary.t; 
	state:State.t } 
  val pushi : t -> int -> t
  val pushf : t -> float -> t
  val lookup : t -> Name.t -> Word.t
end = struct
  type t = 
      { istack:int Stack.t; 
	fstack:float Stack.t; 
	cells:Cell.t array; 
	dict:Dictionary.t; 
	state:State.t }
  let create heap_size = { 
    istack = Stack.create(); 
    fstack = Stack.create(); 
    cells  = Array.create heap_size Value.Empty; 
    dict   = Dictionary.create();
    state  = State.Interpreting;
  }
  open Stack
  let pushi model v = { model with istack=push v model.istack }
  let pushf model v = { model with fstack=push v model.fstack }
  let lookup model = Dictionary.lookup model.dict
end

module Run = struct
  open Parser
  open State
  open Model
  let run model token = 
    match model.state with
      | Interpreting -> 
	(match token with
	  | Token.Integer value -> pushi model value
	  | Token.Float value -> pushf model value
	  | Token.Word name -> 
	    let word = lookup model name in
	      model)
end
