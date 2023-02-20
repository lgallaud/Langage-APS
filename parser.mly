%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA

%token POVI DBPO VIRG ETOI FLEC
%token CONS FUNC REC

%token VAR PROC SET
%token IFINST WHILE CALL

%token IF AND OR BOOL INT

%token ADR
%token VARMinus

%token ECHO

%token <string> OPUN
%token <string> OPBI 



%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds

%type <Ast.block> prog

%type <Ast.argp list> argsp
%type <Ast.exprp list> exprsp

%start prog

%%

prog: block;              { $1 }

block: LBRA cmds RBRA    { ASTBlock($2) }
;

cmds:
  stat                  { [ASTStat $1] }
| def POVI cmds	 { (ASTCmdDef $1)::$3 }
| stat POVI cmds	 { (ASTCmdStat $1)::$3 }
;
 

stat:
  ECHO expr             { ASTEcho($2) }
| SET IDENT expr        { ASTSet($2,$3) }
| IFINST expr block block  { ASTIfInst($2,$3,$4) }
| WHILE expr block      { ASTWhile($2, $3) }
| CALL IDENT exprsp     { ASTCall($2,$3) }
;

expr:
  NUM                         { ASTNum($1) }
| IDENT                       { ASTId($1) }
| LPAR OPUN expr RPAR         { ASTOpun($2, $3) }
| LPAR OPBI expr expr RPAR    { ASTOpbin($2, $3, $4) }
| LPAR expr exprs RPAR        { ASTApp($2, $3) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR     { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR      { ASTOr($3, $4) }
| LBRA args RBRA expr         { ASTAbs($2, $4) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;


  
ntype :
  BOOL    { ASTBool }
| INT     { ASTInt }
| LPAR types FLEC ntype RPAR { ASTAppl($2, $4) }
;

types :
  ntype     { [$1] }
| ntype types { $1::$2 }
;
  
arg :
  IDENT DBPO ntype { ASTArg($1, $3) }
;

args :
  arg        { [$1] }
  |arg VIRG args  { $1::$3 }
;
  
def:
  CONS IDENT ntype expr                        { ASTIdent($2,$3,$4) }
  | FUNC IDENT ntype LBRA args RBRA expr       { ASTFun($2,$3,$5,$7) }
  | FUNC REC IDENT ntype LBRA args RBRA expr  { ASTFunrec($3,$4,$6,$8) }
  | VAR IDENT ntype                            { ASTVar($2, $3) }
  | PROC IDENT LBRA argsp RBRA block            { ASTProc($2,$4,$6) }
  | PROC REC IDENT LBRA argsp RBRA block        { ASTProcRec($3,$5,$7) }
;

argsp:
  argp                              { [$1] }
| argp VIRG argsp                 { $1::$3 }
;

argp:
  IDENT DBPO ntype                 { ASTArgp($1, $3) }
| VARMinus IDENT DBPO ntype        { ASTArgVarp($2, $4) }     
;


exprsp:
  exprp                         { [$1] }  
| exprp exprsp                  { $1::$2 }  
;

exprp:
  expr                          { ASTExprpSeul($1) }  
| LPAR ADR IDENT RPAR           { ASTExprp($3) }   
;  
  

