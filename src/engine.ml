module State = struct
  type t = Interpreting | Compiling
end

module Handle = struct
  type t = int
end

module Name = struct
  type t = string
end

module rec OpCode = struct
  type t = PushInt of int | PushFloat of float | PushLabel of Code.t | Call of Name.t
end

and Code = struct
  type t = Opcode.t list
end

module Ref = struct
  type t = int
end

module Value = struct
  type t = Int of int | Float of int | Ref of Ref.t
end

module Cell = struct
  type t = Value.t
end

module Word = struct
  type kind = Immediate | Executed
  type t = { name:Name.t; kind:kind; OpCode.t list }
end

module Dictionary = struct
  type entry = Word.t list
  type t = (Name.t, entry) Hashtbl.t
end

module Model = struct
  type t = { istack:int Stack.t; fstack:float Stack.t; cells:Cell.t array; dict:Dictionary.t }
end

module Interpret = struct
end
