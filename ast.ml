(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type ntype = ASTBool | ASTInt
             |ASTAppl of ntype list * ntype
                   
and  arg = ASTArg of string * ntype

and  expr =
    ASTNum of int
  | ASTId of string
  | ASTOpun of string * expr
  | ASTOpbin of string * expr * expr
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTAbs of arg list * expr

                      
and  def =
    ASTIdent of string * ntype * expr 
  | ASTFun of string * ntype * arg list * expr
  | ASTFunrec of string * ntype * arg list * expr
  | ASTVar of string * ntype
  | ASTProc of string * argp list * block
  | ASTProcRec of string * argp list * block

and  stat =
    ASTEcho of expr
  | ASTSet of string * expr
  | ASTIfInst of expr * block * block 
  | ASTWhile of expr * block
  | ASTCall of string * exprp list


and  cmd =
    ASTStat of stat
  | ASTCmdDef of def
  | ASTCmdStat of stat

and  block = 
  | ASTBlock of cmd list


and argp =
  | ASTArgp of string * ntype
  | ASTArgVarp of string * ntype 

and exprp =
  | ASTExprp of string
  | ASTExprpSeul of expr

  


