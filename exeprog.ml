open Ast

type value = InZ of int 
            | InF of expr * arg list * (string * value) list 
            | InFR of expr * string * arg list * (string * value) list
            | InA of int
            | Any
            | InP of block * argp list * (string * value) list
            | InPR of  block * string * argp list * (string * value) list


let rec eval_cmds (ctx, mem, ret) cs = 
  match cs with
  | (ASTStat s)::[] -> eval_stat (ctx, mem, ret) s 
  | (ASTCmdDef d)::xs -> let (ctx', mem') = eval_def (ctx, mem) d in
          eval_cmds (ctx', mem', ret ) xs
  | (ASTCmdStat s)::xs -> let (mem', ret') = eval_stat (ctx, mem, ret) s in
          eval_cmds(ctx, mem', ret') xs
  | _ -> failwith "erreur eval cmds"

and eval_bloc (ctx, mem, ret) cs = 
  match cs with 
    ASTBlock cs -> eval_cmds (ctx, mem, ret) cs  
                                    


and eval_stat (ctx, mem, ret) s =
  match s with 
  | ASTEcho e -> (match (eval_exp (ctx, mem) e) with 
        InZ(n) -> (mem, n::ret)
        | _ -> failwith "erreur echo : argument différent de Inz"
  )
  | ASTSet (i,e) -> (let res = (trouver_val ctx i) in (match res with
                        | InA a -> let resEval = (eval_exp (ctx, mem) e) in 
                                          (modifier_memoire mem a resEval, ret)
                        | _ -> failwith "erreur set interdit"
                        )
  )
  | ASTIfInst (e, b1, b2) -> (match (eval_exp (ctx, mem) e) with 
                                | InZ (1) -> eval_bloc (ctx, mem, ret) b1
                                | InZ (0) -> eval_bloc (ctx, mem, ret) b2
                                | _ -> failwith "erreur instruction IF, mauvaise condition"
  )
  | ASTWhile (e, b) -> (match (eval_exp(ctx, mem) e) with
                          | InZ (1) -> let (mem', ret') = eval_bloc (ctx, mem, ret) b in
                                          eval_stat (ctx, mem', ret') (ASTWhile (e, b))
                          | InZ (0) -> (mem, ret)
                          | _ -> failwith "erreur instruction IF, mauvaise condition"
  )
  | ASTCall (id, ex) -> (let res = (trouver_val ctx id) in (match res with
                            | InP (b, argls, ctx') -> 
                                (eval_bloc (ajout_multiple_contexte_proc(ctx, ctx', mem, argls, ex), mem, ret ) b)
                            | InPR(b, idp, argls, ctx') -> (eval_bloc (
                                      ajout_multiple_contexte_proc(ctx , ctx', mem,
                                      argls, 
                                      ex
                                      ) @ [(idp, InPR(b, idp, argls, ctx'))]
                              
                                      , mem, ret
                                      )  b)  
                             | _ -> failwith "erreur ASTCall, pas une procedure"         
                        )
  )


  
(* trouver_memoire mem a *)





  (*| ASTIfInst (e, b1, b2) -> (match (eval_exp ctx e) 

  )*)
(*  | ASTSet (s,e) -> (match ) *)


and eval_def (ctx, mem) d = 
  match d with
  | ASTIdent (id, _, e) -> ( (id, (eval_exp (ctx, mem) e)) :: ctx , mem)
  | ASTFun (id, _, ls, e) -> ( (id, InF (e, ls, ctx)) :: ctx , mem)
  | ASTFunrec (id, _, ls, e) -> ( (id, InFR (e, id, ls, ctx )) :: ctx, mem)
  | ASTVar (id, _) -> (let (a, mem') = alloc_memoire mem in 
                                  ( (id, InA(a)) :: ctx , mem')  )
  | ASTProc (id, args, b) -> ( (id, InP (b, args, ctx)) :: ctx, mem)
  | ASTProcRec (id, args, b) -> ( (id, InPR(b, id, args, ctx)) :: ctx, mem)
                                 
                                  


and trouver_val ctx i =
  match ctx with
  |[] -> failwith "trouver val"
  | (id, exp)::xs -> if i = id then exp else trouver_val xs i

and trouver_memoire mem i =
  match mem with
  |[] -> failwith "trouver mem, valeur non attribuée"
  | (id, exp)::xs -> if i = id then exp else trouver_memoire xs i

and alloc_memoire mem = let a = List.length mem in (a, ((a, Any)::mem) )

and modifier_memoire mem i newval =
  match mem with 
  |[] -> failwith "modifier mem, valeur non attribuée"
  | (id, exp)::xs -> if i = id then (id, newval)::xs else (id, exp) :: modifier_memoire xs i newval

and eval_exp (ctx, mem) e = 
  match e with
  | ASTId "true" -> InZ(1)
  | ASTId "false" -> InZ(0)
  | ASTId i -> let res = (trouver_val ctx i) in (match res with
                                                  | InA a -> trouver_memoire mem a
                                                  | _ -> res
  )

(*| ASTId i -> trouver_val ctx i*)

  | ASTNum n -> InZ(n)  
  | ASTOpun (s, e) -> (match s with "not" -> (match (eval_exp (ctx, mem) e) with
                                           InZ(0) -> InZ(1)
                                          |InZ(1) -> InZ(0) 
                                          | _ -> failwith "erreur not"
                                          )
                                    | _ -> failwith "erreur operation unaire"
  )                    
  | ASTOpbin (s, e1, e2) -> (match s with 
          "eq" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) -> if n1==n2 then InZ(1) else InZ(0)
                                  | _ -> failwith "erreur eq"
          )
          | "lt" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) -> if n1<n2 then InZ(1) else InZ(0)
                                  | _ -> failwith "erreur eq"
          )
          | "add" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) ->  InZ(n1 + n2) 
                                  | _ -> failwith "erreur eq"
          )
          | "sub" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) ->  InZ(n1 - n2) 
                                  | _ -> failwith "erreur eq"
          )
          | "mul" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) ->  InZ(n1 * n2) 
                                  | _ -> failwith "erreur eq"
          )
          | "div" -> (match (eval_exp (ctx, mem) e1), (eval_exp (ctx, mem) e2) with
                                  InZ(n1), InZ(n2) ->  if n2 == 0 
                                                        then failwith "division par zero"
                                                        else InZ(n1 / n2) 
                                  | _ -> failwith "erreur eq"                      
          )
          | _ -> failwith "erreur operation binaire non définie"
  )
                      
  | ASTAnd (e1, e2) -> (match (eval_exp (ctx, mem) e1, eval_exp (ctx, mem) e2) with
      | (InZ(1), InZ(1)) -> InZ(1)
      | _ -> InZ(0)
  )
  | ASTOr (e1, e2) -> (match (eval_exp (ctx, mem) e1, eval_exp (ctx, mem) e2) with
      | (InZ(0), InZ(0)) -> InZ(0)
      | _ -> InZ(1)
  )
  | ASTIf (c, e1, e2) -> (match (eval_exp (ctx, mem) c) with
                      | InZ(1) -> eval_exp (ctx, mem) e1
                      | InZ(0) -> eval_exp (ctx, mem) e2
                      | _ -> failwith "Erreur if"                    
  )           
  | ASTAbs (argls, e) -> InF(e, argls, ctx) 

  | ASTApp (e, ex ) -> ( match (eval_exp (ctx, mem) e) with 
                      | InF(e', argls, ctx') -> 
                            (eval_exp (ajout_multiple_contexte(ctx, ctx', mem, argls, ex), mem ) e')
                      | InFR(e', x, argls, ctx') -> 
                            (eval_exp (
                              ajout_multiple_contexte(ctx , ctx', mem,
                              argls, 
                              ex
                              ) @ [(x, InFR(e', x, argls, ctx'))]
                              
                              , mem
                              )  e')  
                      | _ -> failwith "erreur app"           

  )                   

and eval_expar (ctx, mem) i =
  match i with
  | ASTExprpSeul n -> eval_exp (ctx, mem) n
  | ASTExprp n -> trouver_val ctx n


  (*ASTFunrec (id, _, ls, e) -> (id, InFR (e, eval_arg id, ls, ctx )) :: ctx*)
and eval_argp a =
  match a with 
  | ASTArgp (s,_) -> s
  | ASTArgVarp (s, _) ->  s



and eval_arg (a) =
  match a with 
  ASTArg(s, _) -> s

and ajout_multiple_contexte (ctx, ctx', mem, args, exprs) =
  match args, exprs with
  | [], [] -> ctx' 
  | a::ax, e::ex -> ajout_multiple_contexte ( ctx, (eval_arg a, eval_exp (ctx, mem) e)::ctx', mem, ax, ex) 
  | _ -> failwith "erreur ajout_multiple_contexte, nombre d'argument et d'expression probablement différent"

and ajout_multiple_contexte_proc (ctx, ctx', mem, args, exprs) =
  match args, exprs with
  | [], [] -> ctx' 
  | a::ax, e::ex -> ajout_multiple_contexte_proc ( ctx, (eval_argp a, eval_expar (ctx, mem) e)::ctx', mem, ax, ex) 
  | _ -> failwith "erreur ajout_multiple_contexte, nombre d'argument et d'expression probablement différent"



(*and fonction_x ( l ) =
  match l with
  | [] -> []
  | (p1,t1) :: xs -> *)



let print_output (ctx,ret) = match ret with 
| [] -> Printf.printf"vide"
| x::xs -> Printf.printf"%d\n" x



let eval_prog p = print_output (eval_bloc ([], [], []) p) 


(*let eval_prog p = 
  let (_, sortie) = List.fold_left eval_cmds ([], []) p in
  sortie 
*)

(*
let print_output (ctx,ret) =
  let sortie = eval_prog p in
    Printf.printf (List.hd sortie)

*)

let _ =
try
let ic = open_in Sys.argv.(1) in
  
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p
      (*let l = (eval_prog p) in
        List.hd (l)*)
with Lexer.Eof ->
    exit 0
      
