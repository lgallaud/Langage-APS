(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast



   
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTOpun(s, e) -> (
      Printf.printf"app(id(";
	      Printf.printf "%s)" s ;
	      Printf.printf", [";
	      print_expr e;
	      Printf.printf"])"
    )
    | ASTOpbin(s, e1, e2) -> (
      Printf.printf"app(id(";
	      Printf.printf "%s)" s ;
	      Printf.printf", [";
	      print_expr e1;
        Printf.printf", ";
        print_expr e2;
	      Printf.printf"])"
    )
    | ASTApp(e, es) -> (
	      Printf.printf"app(";
	      print_expr e;
	      Printf.printf",[";
	      print_exprs es;
	      Printf.printf"])"
    )
    | ASTIf(c, p, s) -> (
        Printf.printf"ife(";
        print_expr c;
        Printf.printf",";
        print_expr p;
        Printf.printf",";
        print_expr s;
        Printf.printf")";
    )
    | ASTAnd(a, b) -> (
        Printf.printf"and(";
        print_expr a;
        Printf.printf",";
        print_expr b;
        Printf.printf")";
    )
    | ASTOr(a, b) -> (
        Printf.printf"or(";
        print_expr a;
        Printf.printf",";
        print_expr b;
        Printf.printf")";
    )
    | ASTAbs(a,e) -> (
        Printf.printf"abs([";
        print_args a;
        Printf.printf"], ";
        print_expr e;
        Printf.printf")";
    )

                    
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
	print_expr e;
	print_char ',';
	print_exprs es
    )

and print_type t =
  match t with
    ASTBool -> Printf.printf"bool "
  | ASTInt -> Printf.printf"int "
  | ASTAppl (tl, t) -> (
    Printf.printf"typeFun(";
    print_char '[';
    print_types tl;
    Printf.printf "], ";
    print_type t;
    print_char ')'
  )

and print_types t =
  match t with
    [] -> ()
  | [t] -> print_type t
  | t::tl -> (
      print_type t;
      Printf.printf", ";
      print_types tl
  )
          
and print_arg a =
  match a with
    ASTArg (i, t) -> (
    Printf.printf"(id(%s), " i;
    print_type t;
    Printf.printf")"
  )

and print_args a =
  match a with
    [] -> ()
  | [a] -> print_arg a
  | a::al -> (
      print_arg a;
      print_char ',';
      print_args al
  )
             

and print_stat s =
  match s with
      ASTEcho e -> (
	      Printf.printf("echo(");
	      print_expr(e);
	      Printf.printf(")")
  )
      |ASTSet (s, e) -> (
        Printf.printf"set(%s, " s;        
        print_expr(e);
      	Printf.printf(")")
      )
      |ASTIfInst (e, s1, s2) -> (
        Printf.printf"ifinst(";
        print_expr(e);
        Printf.printf",";
        print_block(s1);
        Printf.printf",";
        print_block(s2);
      	Printf.printf(")")
      )
      |ASTWhile (e, s) -> (
        Printf.printf"while(";
        print_expr(e);
        Printf.printf",";
        print_block(s);
      	Printf.printf(")")
      )
      |ASTCall (s, e) -> (
        Printf.printf"call(%s, [" s;        
        print_exprps(e);
      	Printf.printf("])")
      )






and print_def d =
  match d with
    ASTIdent (n, t, e) -> (
      Printf.printf"const(";
      Printf.printf"id(%s), [ " n;
      print_type t;
      Printf.printf"], (";
      print_expr e;
      Printf.printf") )"
  )
    |ASTFun (n, t, a, e) -> (
      Printf.printf"fun( ";
      Printf.printf"id(%s), [ " n;
      print_type t;
      Printf.printf"], [";
      print_args a;
      Printf.printf"], (";
      print_expr e;
      Printf.printf") )"
  )
    |ASTFunrec (n, t, a, e) -> (
      Printf.printf"funrec( ";
      Printf.printf"id(%s), [ " n;
      print_type t;
      Printf.printf"], [";
      print_args a;
      Printf.printf"], (";
      print_expr e;
      Printf.printf") )"  
  )
    |ASTVar (n, t) -> (
      Printf.printf"var(";
      Printf.printf"id(%s),  " n;
      print_type t;
      Printf.printf" )"
  )
    |ASTProc (n, a, b) -> (
      Printf.printf"proc( ";
      Printf.printf"id(%s), [ " n;
      print_argsp a;
      Printf.printf"], (";
      print_block b;
      Printf.printf") )"
  )
    |ASTProcRec (n, a, b) -> (
      Printf.printf"procrec( ";
      Printf.printf"id(%s), [ " n;
      print_argsp a;
      Printf.printf"], (";
      print_block b;
      Printf.printf") )"
  )
                 

and print_cmd c =
  match c with
      ASTStat s -> print_stat s
    | ASTCmdDef d -> print_def d
    | ASTCmdStat s -> print_stat s
	
and print_cmds cs =
  match cs with
      c::[] -> print_cmd c
    | c::lc -> (
      (*Printf.printf"cmds(";*)
      print_cmd c;
      Printf.printf", ";
      print_cmds lc;
      (*Printf.printf")";*)
    )
    | _ -> failwith "cs vide"
	
and print_block b =
  match b with
    | ASTBlock b -> (
      Printf.printf("[");
      print_cmds b;
      Printf.printf("]")
    )


and print_argp a =
    match a with
      ASTArgp (i, t) -> (
      Printf.printf"(id(%s), " i;
      print_type t;
      Printf.printf")"
    )
    | ASTArgVarp (i, t) -> (
      Printf.printf"var(id(%s), " i;
      print_type t;
      Printf.printf")"
    )
  
and print_argsp a =
  match a with
      [] -> ()
    | [a] -> print_argp a
    | a::al -> (
        print_argp a;
        print_char ',';
        print_argsp al
    )

and print_exprp e =
  match e with
    ASTExprpSeul i -> (
      print_expr i
    )    
    | ASTExprp i -> (
      Printf.printf"var(";
      Printf.printf "%s" i;
      Printf.printf ")";
    )


and print_exprps es =
  match es with
      [] -> ()
    | [e] -> print_exprp e
    | e::es -> (
      print_exprp e;
      print_char ',';
      print_exprps es
    )



let print_prog p =
  Printf.printf("prog(");
  print_block p;
  Printf.printf(")")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
