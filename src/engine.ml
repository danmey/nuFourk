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

module Word = struct
    type kind =
      | Macro
      | Compiled

    type code =
      | Core of (unit -> unit) * Type.signature
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

module Dictionary = struct
  open Hashtbl

  type t = (string, Word.t) Hashtbl.t
  let create() = (Hashtbl.create 1000)
  let lookup = find
  let add dict word = add dict word.Word.name word
end


(*and Types : sig

  type t = {
    return    : U.t list;
    arguments : U.t list;
  }


  (* val signature_of_code : Model.t -> Code.opcode list -> t *)
  (* val signature_of_word : Model.t -> Word.t -> t *)
  val print : t -> unit

end = struct

  open Code
  open BatList

  type t = {
    return    : U.t list;
    arguments : U.t list;
  }

(*      
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
*)

  let print {
    return = return;
    arguments = arguments } =
    print_string "( ";

    for i = 0 to List.length arguments-1 do
      print_string **> U.to_string **> List.nth arguments i;
	print_string " ";
    done;
u1
    print_string "-> ";

    for i = 0 to List.length return-1 do
      print_string **> U.to_string **> (List.nth return i);
      print_string " ";
    done;

    print_string ")";

end
*)
module rec Model : sig

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
  val model          : t
  val push_int       : int -> unit
  val pop_int        : unit -> int
  val push_float     : float -> unit
  val pop_float      : unit -> float
  val push_code      : Code.opcode list -> unit
  val pop_code       : unit -> Code.opcode list
  val pop_value      : unit -> Value.t
  val add_word       : Word.t -> unit
  val get_core_dict  : unit -> Type.dictionary
  val lookup_symbol  : string -> Word.t
  val append_opcode  : Code.opcode -> unit

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
  let heap_size = 1000
  let model = {
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

  let get_core_dict () =
    Hashtbl.fold (fun name word acc ->
      match word.Word.code with
	| Word.Core (_, signature) -> (name, signature) :: acc
	| _ -> acc) model.dict []

end
and Run : sig

  val execute_word   : Model.t -> Word.t -> unit
  val execute_code   : Code.opcode list -> unit
  val execute_symbol : string -> unit
  val run            : Lexer.Token.t -> unit
  val start          : unit -> Model.t

end = struct

  open Lexer

  let rec execute_word model word =
    match word.Word.code with
      | Word.Core (f,_) -> f()
      | Word.User code -> execute_code code

  and execute_code value =
    List.iter
      (function
	| Code.PushInt v   -> Model.push_int v
	| Code.PushFloat v -> Model.push_float v
	| Code.PushCode v  -> execute_code v
	| Code.Call w      -> execute_symbol w) value

  and execute_symbol symbol =
    let w = Model.lookup_symbol symbol
    in
      execute_word Model.model w

  let run token =
    let top_er desc = Printf.printf "TOPLEVEL: %s\n" desc; flush stdout in

      begin
	try
	  match Model.model.Model.state with
	    | Model.Interpreting ->
	      (match token with
		| Token.Integer value -> Model.push_int value;()
		| Token.Float value -> Model.push_float value;()
		| Token.Word name -> execute_symbol name)
	    | Model.Compiling ->
	      (match token with
		| Token.Integer v -> Model.append_opcode **> Code.PushInt v
		| Token.Float v -> Model.append_opcode **> Code.PushFloat v
		| Token.Word "!" -> Model.append_opcode **> Code.App
		| Token.Word name ->
		  let w = Model.lookup_symbol name in
		    (match w.Word.kind with
		      | Word.Macro -> ignore(execute_symbol name)
		      | Word.Compiled -> Model.append_opcode **> Code.Call name))
	with
	  | Error.Runtime_Type str -> top_er str
	  | Error.Symbol_Not_Bound str -> top_er str
	  | Error.Stack_Underflow -> top_er "Stack underflow!"
      end

  let start() =
    Boostrap.init Model.model;
    let rec loop () = 
      let token = Lexer.next_token Model.model.Model.lexbuf in
	run token;
	loop ()
    in
      loop ()

end

and Boostrap : sig

  val init : Model.t -> Model.t

end = struct

  open Model
  open Type
    
  let swap (a,b) = (b,a)
  let init model =
    let app_pair f =
      swap (f (), f ())
    in

    let app2 op pushv pop =
      let (a, b) = app_pair pop in
	op a b |> pushv
    in

    let app2i op () = app2 op push_int pop_int in

    let app2f op () = app2 op push_float pop_float in

    let lift1 f pop model =
      let a = pop () in
	f a
    in

    let lift1i f model = lift1 f pop_int model in
    let lift1f f model = lift1 f pop_float model in
    let lift1c f model = lift1 f pop_code model in

    let tok f = 
      match Lexer.next_token Model.model.Model.lexbuf with
	| Lexer.Token.Word w -> f w
	| _ -> raise (Error.Parse_Error "Expected token `name' not token `value'")
    in

    let tok1 f model = match Lexer.next_token Model.model.Model.lexbuf with
      | Lexer.Token.Word w -> f model w; model
      | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'")
      
    in

    let with_flush f () = f (); flush stdout in

    let st = List.map (fun x -> U.Term (x,[]))
    in
      
    let tsig i o = { Type.input = i; Type.output = o; } in
    let sig_bin_op typ = tsig [ typ; typ ] [ typ ] in
    let macro name signature body = Word.def name Word.Macro signature body in
    let def name signature body = Word.def name Word.Compiled signature body in
    let def_bin_op name typ body = def name (sig_bin_op typ) body in
      [
	def_bin_op "+" int_type **> app2i ( + );
	def_bin_op "-" int_type **> app2i ( - );
	def_bin_op "/" int_type **> app2i ( / );
	def_bin_op "*" int_type **> app2i ( * );

	def_bin_op "f+" float_type **> app2f ( +. );
	def_bin_op "f-" float_type **> app2f ( -. );
	def_bin_op "f/" float_type **> app2f ( /. );
	def_bin_op "f*" float_type **> app2f ( *. );
	def "swap" (tsig [U.Var "a"; U.Var "b"] [U.Var "b"; U.Var "a"]) (fun () -> push_int 1);
	def "i" (tsig [] [int_type]) (fun () -> push_int 1);
	def "f" (tsig [] [float_type]) (fun () -> push_float 1.);

	def "if->i" (tsig [int_type;float_type] [int_type]) (fun () -> ());
	def "if->f" (tsig [int_type;float_type] [float_type]) (fun () -> ());
	def "->i" (tsig [int_type] []) (fun () -> ());
	def "->f" (tsig [float_type] []) (fun () -> ());
	def "i->f" (tsig [int_type] [float_type]) (fun () -> ());
	def "f->i" (tsig [float_type] [int_type]) (fun () -> ());
	def "f->if" (tsig [float_type] [int_type;float_type]) (fun () -> ());


	def "." ( tsig [ int_type ] [ ] ) **>
	  with_flush (fun () ->
	    let v = Model.pop_int () in
	      Printf.printf "%d" v);
	    
	macro "[" void_signature **> (fun () -> Stack.push (ref []) model.codebuf; model.state <- Compiling);
	macro "]" (tsig [] [ U.Term ("code", [U.Var "a"; U.Var "b"]) ]) **> 
	  (fun () -> 
	    let code = !(Stack.pop model.codebuf) in
	      (*	  Type.signature_of_code model **> List.rev **> code; *)
	      (if Stack.is_empty model.codebuf then (model.state <- Interpreting; push_code) else
		  (fun l -> append_opcode **> Code.PushCode l)) **> List.rev code);
	
(*
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
*)
      def "!"  (tsig [U.Term ("code", [U.Var "a";U.Var "b"])] [U.Var "b"] ) **> lift1c **>
	(fun code ->
	  Run.execute_code code
	);

      def "words" void_signature **> with_flush (fun () ->
	let dict = Model.get_core_dict () in
	  List.iter (fun (name, signature) -> Printf.printf "%s :: %s\n" name (Type.signature_to_string signature)) dict);

      def "check" (tsig [U.Term ("code", [U.Var "a";U.Var "b"])] []) **> lift1c **>
	(fun code ->
	  print_endline **> Type.signature_to_string **> Type.signature_of_code (Model.get_core_dict ()) code;
	  flush stdout;
	);

      def "drop" ( tsig [ U.Var ( "a" ) ] [ ] ) **> (fun () -> ignore( Model.pop_value()));
      def "type" void_signature **> (fun () ->
	  let name = match Lexer.next_token Model.model.Model.lexbuf with
	    | Lexer.Token.Word w -> w
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'") 
	  in
	  let word = lookup_symbol name in
	  let signature = match word.Word.code with | Word.Core (_, s) -> s | _ -> failwith "!!" in
	  let s = Type.signature_to_string signature in
	    print_endline s);

	def "type2" void_signature **> with_flush (fun () ->
	  let name1 = match Lexer.next_token Model.model.Model.lexbuf with
	    | Lexer.Token.Word w -> w
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'") 
	  in
	  let name2 = match Lexer.next_token Model.model.Model.lexbuf with
	    | Lexer.Token.Word w -> w
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'") 
	  in
	  let word1 = lookup_symbol name1 in
	  let word2 = lookup_symbol name2 in
	  let signature1 = match word1.Word.code with | Word.Core (_, s) -> s | _ -> failwith "!!" in
	  let signature2 = match word2.Word.code with | Word.Core (_, s) -> s | _ -> failwith "!!" in
	    print_endline **> signature_to_string (Type.check_pair [] signature1 signature2)
	);
      ] |> List.iter add_word;

      Model.model

end

let main () =  Run.start()

