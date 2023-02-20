(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'		           { POVI }
  | ':'              { DBPO }
  | ','              { VIRG }
  | '*'              { ETOI }
  | "->"             { FLEC }
  | "CONST"          { CONS }
  | "FUN"            { FUNC }
  | "REC"            { REC }
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "ECHO"           { ECHO }
  | "SET"            { SET }
  | "IF"             { IFINST }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  | "bool"           { BOOL }
  | "int"            { INT }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | "not"             as lxm { OPUN(lxm) }
  | "eq"              as lxm { OPBI(lxm) }
  | "lt"              as lxm { OPBI(lxm) }
  | "add"              as lxm { OPBI(lxm) }
  | "sub"              as lxm { OPBI(lxm) }
  | "mul"              as lxm { OPBI(lxm) }
  | "div"              as lxm { OPBI(lxm) }
  | "var"           { VARMinus }
  | "adr"           { ADR }


  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
