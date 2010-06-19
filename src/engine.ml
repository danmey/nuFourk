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

module Word = struct
  type kind = Immediate | Executed
  type t = { name:Name.t; kind:kind; code:Opcode.t list }
end

module Dictionary = struct
  type entry = Word.t list
  type t = (Name.t, entry) Hashtbl.t
  let create() = Hashtbl.create 1000
end

module Model = struct
  type t = { istack:int Stack.t; fstack:float Stack.t; cells:Cell.t array; dict:Dictionary.t }
  let create heap_size = { 
    istack = Stack.create(); 
    fstack = Stack.create(); 
    cells  = Array.create heap_size Value.Empty; 
    dict   = Dictionary.create() 
  }
end

module Interpret = struct
end
