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
module rec Word : sig 
  type kind = Immediate | Compiled
  type code = Core of (Model.t -> Model.t) | User of Opcode.t list
  type t = { name:Name.t; code:code; kind:kind; }
  val call : Model.t -> t-> Model.t 
  val core : Name.t -> (Model.t -> Model.t) -> t
end = struct
  type kind = Immediate | Compiled
  type code = Core of (Model.t -> Model.t) | User of Opcode.t list
  type t = { name:Name.t; code:code; kind:kind; }
  let call model word = 
    match word.code with
	Core  f -> f model
  let core name code = { name = name; code = Core code; kind = Compiled }
end
and Dictionary : sig 
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
      { istack:int Stack.t; 
	fstack:float Stack.t; 
	cells:Cell.t array; 
	dict:Dictionary.t; 
	state:State.t } 
  val create : int -> t
  val pushi : t -> int -> t
  val pushf : t -> float -> t
  val popi : t -> t * int
  val add : t -> Word.t -> t
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
  let popi model = let v, s = pop model.istack in { model with istack=s },v
  let lookup model = Dictionary.lookup model.dict
  let add model word = Dictionary.add model.dict word ; model
end

module Boostrap = struct
  open Model
  open Word
  let plus = core "+" (fun model -> let model,a = popi model in let model, b = popi model in pushi model (a+b))2
  let dot = core "." (fun model -> let model,a = popi model in print_int a; flush stdout; model)
  let init model = add model plus; add model dot
end

module Run = struct
  open Parser
  open State
  open Model
  open Word
  let run model token = 
    match model.state with
      | Interpreting -> 
	(match token with
	  | Token.Integer value -> pushi model value
	  | Token.Float value -> pushf model value
	  | Token.Word name -> 
	    let word = lookup model name in
	      call model word)
end

let init() = 
  let model = Model.create 1000 in
    Boostrap.init model
