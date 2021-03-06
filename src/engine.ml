(*---------------------------------------------------------------------------- 
nuFourk - Concatenative, stack based, functional, strictly typed language.
Copyright (C) 2010 Wojciech Meyer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------*)

open BatLexing
open BatPervasives
open BatOption
open BatList
open Unify
  
module Error = struct
  exception Runtime_Type of string
  exception Stack_Underflow
  exception Symbol_Not_Bound of string
  exception Parse_Error of string
end
    
module Value = struct
  
  type t =
    | Int of int
	  
    | Float of float
    | Bool of bool
    | String of string
    | Empty
    | Code of Code.opcode list
	  
  let to_string =
    function
      | Int   _ -> "int"
      | Float _ -> "float"
      | Bool _ -> "bool"
      | String _ -> "string"
      | Empty   -> "empty"
      | Code  _ -> "code"
end
    
module Cell = struct
  type t = Value.t
end
    
module Word = struct
  type kind =
    | Macro
    | Compiled
	
  type code =
    | Core of (unit -> unit) * Type.signature
    | User of Code.opcode list * Type.signature
	  
  type t = {
      name : string;
      code : code;
      kind : kind;
    }
	
  let def name kind s code = {
    name = name;
    code = Core (code,s);
    kind = kind }
      
  let def_user name code signature = {
    name = name;
    code = User (code, signature);
    kind = Compiled }
      
  let empty name = {
    name = name;
    code = User ([], Type.void_signature);
    kind = Compiled }
end
    
module Dictionary = struct
  open Hashtbl
    
  type t = (string, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  let lookup = find
  let add dict word = add dict word.Word.name word
end

module rec Model : sig
  
  type state =
    | Interpreting
    | Compiling
	
  type t = {
      env    : (string * Value.t) list;
      stack  : Cell.t Stack.t;
      cells  : Cell.t array;
      dict   : Dictionary.t;
      lexbuf : Lexing.lexbuf;
      codebuf: Code.opcode list ref Stack.t;
      mutable state: state;
    }
  val model          : t
  val push_int       : int -> unit
  val pop_int        : unit -> int
  val push_float     : float -> unit
  val push_value     : Value.t -> unit
  val pop_float      : unit -> float
  val push_code      : Code.opcode list -> unit
  val push_bool      : bool -> unit
  val push_string      : string -> unit
  val pop_bool      : unit -> bool
  val pop_string      : unit -> string
  val pop_code       : unit -> Code.opcode list
  val pop_value      : unit -> Value.t
  val add_word       : Word.t -> unit
  val get_dict  : unit -> Type.dictionary
  val lookup_symbol  : string -> Word.t
  val append_opcode  : Code.opcode -> unit
  (* val get_variable   : string -> Value.t *)
end = struct
  open Stack
    
  type state =
    | Interpreting
    | Compiling
	
  type t = {
      env    : (string * Value.t) list;
      stack  : Cell.t Stack.t;
      cells  : Cell.t array;
      dict   : Dictionary.t;
      lexbuf : Lexing.lexbuf;
      codebuf: Code.opcode list ref Stack.t;
      mutable state: state;
    }
  let heap_size = 1000
  let model = {
    env     = [];
    stack   = Stack.create();
    cells   = Array.create heap_size Value.Empty;
    dict    = Dictionary.create();
    lexbuf  = from_input stdin;
    codebuf = Stack.create();
    state   = Interpreting }
      
  let push_int i = push (Value.Int i) model.stack
      
  let pop_int () =
    try
      match pop model.stack with
      | Value.Int i -> i
      | a -> raise (Error.Runtime_Type("Expected type `int' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
      Empty -> raise Error.Stack_Underflow
	  
  let push_float f = push (Value.Float f) model.stack
  let push_bool v = push (Value.Bool v) model.stack
  let push_string v = push (Value.String v) model.stack
  let push_value v = push v model.stack
  let push_code f = push (Value.Code f) model.stack
      
  let pop_value () = pop model.stack
  let pop_code () =
    try
      match pop model.stack with
      | Value.Code i -> i
      | a -> raise (Error.Runtime_Type("Expected type `code' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
      Empty -> raise Error.Stack_Underflow
	  
  let pop_float () =
    try
      match pop model.stack with
      | Value.Float i -> i
      | a -> raise (Error.Runtime_Type("Expected type `float' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
      Empty -> raise Error.Stack_Underflow
	  
  let pop_bool () =
    try
      match pop model.stack with
      | Value.Bool i -> i
      | a -> raise (Error.Runtime_Type("Expected type `bool' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
      Empty -> raise Error.Stack_Underflow
	  
  let pop_string () =
    try
      match pop model.stack with
      | Value.String i -> i
      | a -> raise (Error.Runtime_Type("Expected type `string' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
      Empty -> raise Error.Stack_Underflow
	  
  let add_word word =
    Dictionary.add model.dict word; ()
      
  let lookup_symbol name =
    try
      Dictionary.lookup model.dict name
    with
    | Not_found -> raise (Error.Symbol_Not_Bound ( "Symbol `" ^ name ^ "' is not found in this context!"))
	  
  let append_opcode token =
    let l = top model.codebuf
    in
    l := token::!l
		  
  let get_dict () =
    Hashtbl.fold (fun name word acc ->
      match word.Word.code with
      | Word.Core (_, signature) -> (name, signature) :: acc
      | Word.User (_, signature) -> (name, signature) :: acc) model.dict []
      
end
and Run : sig
  
  val execute_word   : Word.t -> unit
  val execute_code   : Code.opcode list -> unit
  val execute_symbol : string -> unit
  val run            : Lexer.Token.t -> unit
      
end = struct
  
  open Lexer
    
  let rec execute_word word =
    match word.Word.code with
    | Word.Core (f,_) -> f ()
    | Word.User (code,_) -> execute_code code
	  
  and execute_code value =
    List.iter
      (function
	| Code.PushInt v    -> Model.push_int v
	| Code.PushFloat v  -> Model.push_float v
	| Code.PushString v -> Model.push_string v
	| Code.PushCode v   -> Model.push_code v
	| Code.PushBool v   -> Model.push_bool v
	| Code.App          -> let code = Model.pop_code() in execute_code code
	| Code.Call w       -> execute_symbol w) value
      
  and execute_symbol symbol =
    let w = Model.lookup_symbol symbol
    in  execute_word w
      
  let run token =
    let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout in
    
    begin
      try
	match Model.model.Model.state with
	| Model.Interpreting ->
	    (match token with
	    | Token.Integer value -> Model.push_int value;()
	    | Token.Float value -> Model.push_float value;()
	    | Token.String value -> Model.push_string value;()
	    (* | Token.Word "$" -> let code = Model.pop_code() in execute_code code *)
	    | Token.Word name -> execute_symbol name)
	| Model.Compiling ->
	    (match token with
	    | Token.Integer v -> Model.append_opcode **> Code.PushInt v
	    | Token.Float v -> Model.append_opcode **> Code.PushFloat v
	    | Token.String v -> Model.append_opcode **> Code.PushString v
	    (* | Token.Word "$" -> Model.append_opcode **> Code.App (\* Uggly *\) *)
	    | Token.Word name ->
		let w = Model.lookup_symbol name in
		(match w.Word.kind with
		| Word.Macro -> ignore(execute_symbol name)
		| Word.Compiled -> Model.append_opcode **> Code.Call name))
      with
      | Error.Runtime_Type str -> top_er str
      | Error.Symbol_Not_Bound str -> top_er str
      | Error.Stack_Underflow -> top_er "Stack underflow!"
      | U.Unify_fail (first,second) -> 
	  top_er (Printf.sprintf "Expected type `%s', found `%s'!" first second)
      | Stack.Empty -> top_er "Stack empty!"
	    
    end
      
      
end
  
    
    
    
    
    
