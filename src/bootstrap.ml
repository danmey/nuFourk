open BatPervasives
open Engine
open Model
open Unify
open Type

let init model =

  let app_pair f = f (), f ()
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
    
  let st = List.map (fun x -> U.Term (x,[])) in
    
  let sign i o = { Type.input = i; Type.output = o; } in

  let sign_bin_op typ = sign [ typ; typ ] [ typ ] in

  let macro name signature body = Word.def name Word.Macro signature body in

  let adef name signature body = Word.def name Word.Compiled signature body in

  let closure_type a b = U.Term ("code", [U.Var a; U.Var b]) in

  let def_bin_op name typ body = adef name (sign_bin_op typ) body in

  let def_bin_op_ret name typ ret body = adef name (sign [ typ; typ ] [ ret ]) body in

  let mk_var ch = U.Var (string_of_char ch) in
  let v a = U.Var (string_of_char a) in
  let mk_vars = List.map mk_var in
  
  let def_poly_op name (consume, produce) = 
    adef name (sign (mk_vars consume) (mk_vars produce)) 
  in

  let def_op name (consume, produce) = 
    adef name (sign consume produce)
  in
    
  let def_const name typ value = 
    adef name (sign [] [bool_type]) (fun () -> push_value value)
  in
  let def = adef in
  let ( --> ) a b = a, b in
  let ( ---> ) a b = [U.Term ("code", [U.Term ("list", a); U.Term ("list", b)])] in
	[
	  def_bin_op "+" int_type **> app2i ( + );
	  def_bin_op "-" int_type **> app2i ( - );
	  def_bin_op "/" int_type **> app2i ( / );
	  def_bin_op "*" int_type **> app2i ( * );
	
	  def_bin_op "f+" float_type **> app2f ( +. );
	  def_bin_op "f-" float_type **> app2f ( -. );
	  def_bin_op "f/" float_type **> app2f ( /. );
	  def_bin_op "f*" float_type **> app2f ( *. );
	  
	  (* Should be expressed as monads *)
	  def_poly_op "swap" 
	    (['a'; 'b'] --> ['b'; 'a']) 
	    (fun () -> 
	      let a, b = pop_value(), 
		pop_value() in 
		push_value b; 
		push_value a);
	  
	  def_poly_op "dup" 
	    (['a'] --> ['a'; 'a']) 
	    (fun () -> 
	      let a = pop_value() in 
		push_value a; 
		push_value a);
	  
	  def_poly_op "rot" 
	    (['a'; 'b'; 'c'] --> ['b'; 'c'; 'a'])
	    (fun () -> 
	      let a, b, c = 
		pop_value(), 
		pop_value(), 
		pop_value() in 
		push_value b; 
		push_value c; 
		push_value a);

	  def_op "=" ([v 'a'; v 'a'] --> [bool_type])
	    (fun () -> 
	      let v1, v2 = 
		pop_value(), 
		pop_value() in 
		push_bool (v1 = v2));
	  
	  def_op "test2" (([v 'A'] ---> [v 'A']) --> ([v 'A'] ---> [v 'A'])) (fun () -> ());
	  
	  def_const "false" bool_type (Value.Bool false);

	  def_const "true" bool_type (Value.Bool true);


	  def "show" (sign [string_type] []) (with_flush (fun () -> let str = pop_string () in print_string str));
      def "." ( sign [ int_type ] [ ] ) **>
	with_flush (fun () ->
	  let v = Model.pop_int () in
	    Printf.printf "%d" v);
      
      macro "[" void_signature **> (fun () -> Stack.push (ref []) model.codebuf; model.state <- Compiling);
      macro "]" (sign [] [ U.Term ("code", [U.Var "a"; U.Var "b"]) ]) **> 
	(fun () -> 
	  let code = !(Stack.pop model.codebuf) in
	  let signature = Type.signature_of_code (Model.get_dict()) **> List.rev **> code in
	    (if Stack.is_empty model.codebuf then (model.state <- Interpreting; push_code) 
	     else
		(fun l -> append_opcode **> Code.PushCode l)) **> List.rev code);
      
      
    (*
      def ".."  { Types.arguments = st ["code"]; Types.return = [] }   **> (fun model ->
      print_string "[ ";
      List.iter (fun x -> Printf.printf "%s " **> Code.to_string x) **> List.rev **> pop_code model;
      print_string "]";
      flush stdout);
    *)
      macro ":"  void_signature **>
	tok1 **>
	(fun model name ->
	  let code = pop_code model in
	  let signature = Type.signature_of_code (Model.get_dict ()) code in
	    add_word **> Word.def_user name code signature;
	);
      
      def_bin_op_ret "<" int_type bool_type (fun () -> 
	let i1, i2 = pop_int(), pop_int() in 
	  push_bool (i1 < i2));
      
      def "?" (sign [closure_type "a" "e"; closure_type "b" "e" ;bool_type] [U.Var "e"])
	(fun () ->
	  let b1 = pop_code() in
	  let b2 = pop_code() in
	  let cond = pop_bool () in
	    if cond then 
	      Run.execute_code b1
	    else 
	      Run.execute_code b2);
      
      def "loop" (sign [ U.Term ("code", [U.Var "e";U.Term ("list", [bool_type])]);closure_type "b" "e"; U.Var "b" ] [U.Var "e"])
	(fun () ->
	  let cond_code = pop_code () in
	  let body = pop_code() in
	  let rec loop () =
	    Run.execute_code cond_code;
	    let cond = pop_bool () in
	      if cond then 
		begin
		  Run.execute_code body;
		  loop()
		end
	      else
		() 
	  in
	    loop ());
      
      def "b." (sign [bool_type] [])
	(with_flush
	   (fun () ->
	     let b = pop_bool () in
	       print_bool b));
      
      
    (*
      def "!"  (sign [U.Var "a"] [U.Var "b"] ) **> lift1c **>
      (fun code ->
      Run.execute_code code
      );
    *)
      def "words" void_signature **> with_flush (fun () ->
	let dict = Model.get_dict () in
	  List.iter (fun (name, signature) -> Printf.printf "%s :: %s\n" name (Type.signature_to_string signature)) dict);
      
      def "check" (sign [U.Term ("code", [U.Var "a";U.Var "b"])] []) **> lift1c **>
	(fun code ->
	  print_endline **> Type.signature_to_string **> (Type.signature_of_code (Model.get_dict ()) code);
	  flush stdout;
	);
      
      def "drop" ( sign [ U.Var ( "a" ) ] [ ] ) **> (fun () -> ignore( Model.pop_value()));
      def "type" void_signature **> (fun () ->
	let name = match Lexer.next_token Model.model.Model.lexbuf with
	  | Lexer.Token.Word w -> w
	  | _ -> raise (Error.Parse_Error "Expected token `name' not token `value'") 
	in
	let word = lookup_symbol name in
	let signature = match word.Word.code with | Word.Core (_, s) -> s | Word.User (_, s) -> s in
	let s = Type.signature_to_string signature in
	  print_endline s);
      

      
      def "key" (sign [] [int_type]) **> 
	(fun () ->
	  let c = int_of_char (input_char stdin) in
	    push_int c);
      
      def "append" (sign [int_type; string_type] [string_type]) **> 
	(fun () ->
	  let c = char_of_int (pop_int()) in
	  let str = pop_string() in
	  let tmp = String.make 1 c in
	    push_string (String.concat "" [str;tmp]));
      
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
	  print_endline **> signature_to_string (snd (Type.check_pair [] signature1 signature2))
      );
    ] |> List.iter add_word;
    Model.model

let start() =
  init Model.model;
  let rec loop () =
    let token = Lexer.next_token Model.model.Model.lexbuf in
      Run.run token;
      loop ()
  in
    loop ()
