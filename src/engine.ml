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
    | Empty
    | Code of Code.opcode list

  let to_string =
    function
      | Int   _ -> "int"
      | Float _ -> "float"
      | Empty   -> "empty"
      | Code  _ -> "code"
end

module Cell = struct
  type t = Value.t
end

module rec Dictionary : sig

  type t

  val lookup : t -> string -> Word.t
  val add    : t -> Word.t -> unit
  val create : unit -> t

end = struct

  open Hashtbl

  type t = (string, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  let lookup = find
  let add dict word = add dict word.Word.name word
end

and Word : sig

    type kind =
      | Macro
      | Compiled

    type code =
      | Core of (Model.t -> unit) * Types.t
      | User of Code.opcode list

    type t = {
      name:string;
      code:code;
      kind:kind;
    }

    val def      : string -> kind -> Types.t -> (Model.t -> unit) -> t
    val def_user : string -> Code.opcode list -> t

end = struct

    type kind =
      | Macro
      | Compiled

    type code =
      | Core of (Model.t -> unit) * Types.t
      | User of Code.opcode list

    type t = {
      name : string;
      code : code;
      kind : kind;
    }

    let def name kind s code = {
      name = name;
      code = Core (code,s);
      kind = kind }

    let def_user name code = {
	name = name;
	code = User code;
	kind = Compiled }

    let empty name = {
      name = name;
      code = User [];
      kind = Compiled }
end

and Types : sig

  type t = {
    return    : U.t list;
    arguments : U.t list;
  }

  val signature_of_code : Model.t -> Code.opcode list -> t
  val signature_of_word : Model.t -> Word.t -> t
  val print : t -> unit

end = struct

  open Word
  open Model
  open Code
  open BatList

  type t = {
    return    : U.t list;
    arguments : U.t list;
  }

      
  let rec signature_of_code model code =
    let arguments = Stack.create() in
    let stack = Stack.create() in
    let normalize_arguments arg_types fun_arg_types =
      let arg_types = (take **> List.length fun_arg_types) **> arg_types in
      let arg_types =
	if List.length arg_types != List.length fun_arg_types then
	  let cut_count = List.length fun_arg_types - List.length arg_types in
	  let cut_args = List.rev **> take cut_count **> List.rev fun_arg_types in
	    arg_types @ cut_args
	else arg_types in
	List.combine arg_types fun_arg_types
    in

    let expect typ =
      if Stack.is_empty stack then
	Stack.push typ arguments
      else
	let _,typ' = Stack.pop stack in
	  try
	    ignore **> List.map U.unify (normalize_arguments typ' typ)
	  with _ ->
	    let l a = String.concat " " **> List.map U.to_string a in
	    raise (Error.Runtime_Type (Printf.sprintf "Expected type `%s', found `%s'!" (l typ) (l typ')))
    in

    let fun_app_check arg_types fun_arg_types =
      let arguments, _ = List.split arg_types in
      let terms = normalize_arguments arguments fun_arg_types in
      let check t =
	try
	  ignore(List.map U.unify t)
	with (U.Unify_fail (n1,n2)) ->
	  raise
	    (Error.Runtime_Type
	       (Printf.sprintf "AAA:Expected type `%s', found `%s'!" n2 n1))
      in
	check terms
    in

    let st nm = [],[U.Term (nm, [])] in

    let to_list st = let ret = ref [] in Stack.iter (fun el -> ret := !ret@[el]) st; !ret in

    let stack_effects = function
	  | PushInt _ -> Stack.push (st "int") stack
	  | PushFloat _ -> Stack.push (st "float") stack
	  | Call name ->
	    let w = lookup_symbol model name in
	    let s = signature_of_word model w in
	      ()
(*	      fun_app_check (to_list stack) s.arguments *)
	  | App ->
	    expect []
    in
      List.iter stack_effects code;
      { arguments = []; return = [] }
(*      { arguments = List.concat ( List.map snd (to_list arguments)); return = List.concat ( List.map snd (to_list stack)) } *)

  and signature_of_word model word =
    match word.Word.code with
      | User code -> signature_of_code model code
      | Core (nm,s) -> s

  let print {
    return = return;
    arguments = arguments } =
    print_string "( ";

    for i = 0 to List.length arguments-1 do
      print_string **> U.to_string **> List.nth arguments i;
	print_string " ";
    done;

    print_string "-> ";

    for i = 0 to List.length return-1 do
      print_string **> U.to_string **> (List.nth return i);
      print_string " ";
    done;

    print_string ")";

end
and Model : sig

  type state =
    | Interpreting
    | Compiling

  type t = {
    stack  : Cell.t Stack.t;
    cells  : Cell.t array;
    dict   : Dictionary.t;
    lexbuf : Lexing.lexbuf;
    codebuf: Code.opcode list ref Stack.t;
    mutable state: state;
  }

  val create         : int -> t
  val push_int       : t -> int -> unit
  val pop_int        : t -> int
  val push_float     : t -> float -> unit
  val pop_float      : t -> float
  val push_code      : t -> Code.opcode list -> unit
  val pop_code       : t -> Code.opcode list
  val add_word       : t -> Word.t -> t
  val lookup_symbol  : t -> string -> Word.t
  val next_token     : t -> (t -> Lexer.Token.t -> t) -> t
  val append_opcode  : t -> Code.opcode -> unit

end = struct
  open Stack

  type state =
    | Interpreting
    | Compiling

  type t = {
    stack  : Cell.t Stack.t;
    cells  : Cell.t array;
    dict   : Dictionary.t;
    lexbuf : Lexing.lexbuf;
    codebuf: Code.opcode list ref Stack.t;
    mutable state: state;
  }

  let create heap_size =
    let m = {
      stack   = Stack.create();
      cells   = Array.create heap_size Value.Empty;
      dict    = Dictionary.create();
      lexbuf  = from_input stdin;
      codebuf = Stack.create();
      state   = Interpreting }
    in
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

  let push_code model f = push (Value.Code f) model.stack

  let pop_code model =
    try
      match pop model.stack with
	| Value.Code i -> i
	| a -> raise (Error.Runtime_Type("Expected type `code' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

  let pop_float model =
    try
      match pop model.stack with
	| Value.Float i -> i
	| a -> raise (Error.Runtime_Type("Expected type `float' value given is of type is `" ^ Value.to_string a ^ "'!"))
    with
	Empty -> raise Error.Stack_Underflow

  let add_word model word =
    Dictionary.add model.dict word;
    model

  let lookup_symbol model name =
    try
      Dictionary.lookup model.dict name
    with
      | Not_found -> raise (Error.Symbol_Not_Bound ( "Symbol `" ^ name ^ "' is not found in this context!"))

  let next_token model kont =
    Run.expect model
      (fun model token ->
	let m = kont model token
	in Run.continue model)

  let append_opcode model token =
    let l = top model.codebuf
    in
      l := token::!l
end
and Run : sig

  val execute_word   : Model.t ->  Word.t -> unit
  val execute_code   : Model.t -> Code.opcode list -> unit
  val execute_symbol : Model.t -> string -> unit
  val run            : Model.t -> Lexer.Token.t -> Model.t
  val expect         : Model.t -> (Model.t -> Lexer.Token.t -> Model.t) -> Model.t
  val continue       : Model.t -> Model.t
  val start          : unit -> Model.t

end = struct

  open Lexer
  open Model
  open Word

  let rec execute_word model word =
    match word.Word.code with
      | Word.Core (f,_) -> f model;()
      | Word.User code -> execute_code model code

  and execute_code model =
    List.iter
      (function
	| Code.PushInt v -> push_int model v
	| Code.PushFloat v -> push_float model v
	| Code.PushCode v -> execute_code model v
	| Code.Call w -> execute_symbol model w)

  and execute_symbol model symbol =
    let w = lookup_symbol model symbol
    in
      execute_word model w

  let run model token =
    let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout in

      begin
	try
	  match model.state with
	    | Interpreting ->
	      (match token with
		| Token.Integer value -> push_int model value;()
		| Token.Float value -> push_float model value;()
		| Token.Word name -> execute_symbol model name)
	    | Compiling ->
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
	  | Error.Stack_Underflow -> top_er "Stack underflow!"
      end;
      model

  let expect model kont =
    Lexer.next_token kont model model.lexbuf

  let continue model = expect model run

  let start() =
    let model = Model.create 1000
    in
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
    let lift1c f model = lift1 f pop_code model in

    let tok f model = Model.next_token model
      (fun model ->
	(function
	  | Lexer.Token.Word w -> f w; model
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'"
	  ))); ()
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

    let macro name signature body = def name Macro signature body in

    let def name signature body = def name Compiled signature body in
      [
	def "+"  { Types.arguments = st ["int"; "int"]; Types.return = st ["int"] }  **> app2i ( + );
(*
	def "f+" { Types.arguments = st ["float";"float"]; Types.return = st ["float"] }  **> app2f ( +. );

(*      def "-" Compiled **> app2 ( - );
      def "*" Compiled **> app2 ( * );
      def "/" Compiled **> app2 ( / );
*)
      def "."   { Types.arguments = st ["int"]; Types.return = [] } **> lift1i **> with_flush print_int;
      def "f."  { Types.arguments = st ["float"]; Types.return = [] } **> lift1f **> with_flush print_float;
      macro "[" { Types.arguments = []; Types.return = [] }**> (fun model -> Stack.push (ref []) model.codebuf; model.state <- Compiling);
      macro "]" { Types.arguments = []; Types.return = [U.Term ("code", [U.Var "a";U.Var "b"])] }    **> (fun model ->
	let code = !(Stack.pop model.codebuf) in
	  Types.signature_of_code model **> List.rev **> code;
	  (if Stack.is_empty model.codebuf then (model.state <- Interpreting; push_code model) else
	      (fun l -> append_opcode model **> Code.PushCode l)) **> List.rev code);

      def ".."  { Types.arguments = st ["code"]; Types.return = [] }   **> (fun model ->
	print_string "[ ";
	List.iter (fun x -> Printf.printf "%s " **> Code.to_string x) **> List.rev **> pop_code model;
	print_string "]";
	flush stdout);
      macro ":"  { Types.arguments = []; Types.return = [] } **>
	tok1 **>
	(fun model name ->
	  let code = pop_code model in
	  Types.signature_of_code model code;
	  Dictionary.add model.dict **> Word.def_user name **> code;
	);
      def "!"  { Types.arguments = [U.Term ("code", [U.Var "a";U.Var "b"])]; Types.return = [U.Var "b"] } **> lift1c **>
	(fun code ->
	  Run.execute_code model code
	);

      def "check" { Types.arguments = [U.Term ("code", [U.Var "a";U.Var "b"])]; Types.return = [] } **> lift1c **>
	(fun code ->
	  Types.print **> Types.signature_of_code model code;
	  flush stdout;
	);
      *)
      macro "type"  { Types.arguments = []; Types.return = [] } **> tok **> with_flush
	(fun name ->
	  let word = lookup_symbol model name in
	  let s = Types.signature_of_word model word in
	    Types.print s; print_endline ""
	)
      ] |> List.fold_left add_word model
end

let main () =  Run.start()

