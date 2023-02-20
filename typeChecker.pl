

main_stdin :- read(user_input, T), typeCheck(T,R), print(R), nl, exitCode(R).

/*============================================*/

resType(_, [], []).
resType(G, [A|Args], [T|Types]) :- 
    typeExpr(G, A, T), 
    resType(G, Args, Types).

liste_to_type([],[]).
liste_to_type([var(_,T)|Q], [ref(T)|L] ) :- liste_to_type(Q, L).
liste_to_type([(_,T)|Q], [T|L] ) :- liste_to_type(Q, L).

matching(_, [], []).
matching(G, [T|Tl], [E|El]) :- typeExpar(G,E,T), matching(G, Tl, El).



getValContexte(X,[(X,T) | _], T).
getValContexte(X,[ _ | Q], T) :-  getValContexte(X, Q, T).

addValContexte(Id, Type, G0, [(Id,Type)|G0]). 

ajoutMultipleListe([], G, G).
ajoutMultipleListe([(id(Id), Type)|As], G0, G) :- 
    addValContexte(Id, Type, G0, Gtemp),
    ajoutMultipleListe(As, Gtemp, G).    
ajoutMultipleListe([var(id(Id), Type)|As], G0, G) :- 
    addValContexte(Id, ref(Type), G0, Gtemp),
    ajoutMultipleListe(As, Gtemp, G).    




memeType(T, T).
memeType([T], T).              
memeType([X|Xs], [Y|Ys]) :- X==Y, memeType(Xs, Ys).


/*a([], []).
a([(X, ref(T) ) | Q ] [ (ref(P), T) |L]) :-  a(Q, L).  
*/




/*============================================*/

/*typeProg(prog(Cs)) :- init(Ctx), typeCmds(Ctx, Cs, void).*/


typeProg(prog(Cs)) :- init(Ctx), typeBlock(Ctx, Cs, void).

typeBlock(G, Cs, void) :- typeCmds(G, Cs, void).


typeCmds(_, [], void).
typeCmds(G,[D|Cs], T) :- typeDef(G, D, G2), typeCmds(G2, Cs, T).
typeCmds(G,[S|Cs], void) :- typeStat(G, S ,void), typeCmds(G, Cs, void).


typeDef(G, const( id(X), T, E), G2) :-     
    typeExpr(G, E, Tsortie), 
    memeType(T, Tsortie),
    addValContexte(X, Tsortie, G, G2).


typeDef(G, fun(id(I), [T], Args, E), G2) :- 
    liste_to_type(Args,TypeL),
    ajoutMultipleListe(Args, G, Gtemp),
    typeExpr(Gtemp, E, Tsortie),
    memeType(T,Tsortie),
    addValContexte(I, typeFun(TypeL,Tsortie), G, G2).

typeDef(G, funrec(id(I), [T], Args, E), G2) :- 
    liste_to_type(Args,TypeL),
    ajoutMultipleListe(Args, G, Gtemp),
    addValContexte(I, typeFun(TypeL,T), Gtemp, Gtemp2),
    typeExpr(Gtemp2, E, T),
    addValContexte(I, typeFun(TypeL,T), G, G2).
     
typeDef(G, var(id(X), int), G2 ) :-
    addValContexte(X, ref(int), G, G2).

typeDef(G, var(id(X), bool), G2 ) :-
    addValContexte(X, ref(bool), G, G2).

typeDef(G, proc(id(I), Args, Bk), G2) :-
    ajoutMultipleListe(Args, G, Gtemp),
    typeBlock(Gtemp, Bk, void),
    liste_to_type(Args,TypeL),
    addValContexte(I, typeProc(TypeL, void), G, G2).

typeDef(G, procrec(id(I), Args, Bk), G2) :-
    liste_to_type(Args,TypeL),
    ajoutMultipleListe(Args, G, Gtemp),
    addValContexte(I, typeProc(TypeL, void), Gtemp, Gtemp2),
    typeBlock(Gtemp2, Bk, void),        
    addValContexte(I, typeProc(TypeL, void), G, G2).



typeStat(G, echo(E), void) :- typeExpr(G, E, int). 
typeStat(G, set(X, E), void) :- typeExpr(G, E, T), getValContexte(X,G,ref(T)).
typeStat(G, ifinst(E, B1, B2), void) :- 
    typeExpr(G, E, bool),
    typeBlock(G, B1, void),
    typeBlock(G, B2, void).

typeStat(G, while(E, S), void) :- typeExpr(G, E, bool), typeBlock(G, S, void).

typeStat(G, call(X, Es), void) :- 
    getValContexte(X, G, typeProc(Args, void)),
    matching(G, Args, Es).


typeExpr().
typeExpr(_, num(_), int).

typeExpr(G, id(X), T) :- getValContexte(X,G,ref(T)).
typeExpr(G, id(X), T) :- getValContexte(X,G,T).

typeExpr(G, ife(C, P, S), T) :- typeExpr(G, C, bool), 
                                typeExpr(G, P, T), typeExpr(G, S, T). 
typeExpr(G, and(A,B), bool) :- typeExpr(G, A, bool), typeExpr(G, B, bool).
typeExpr(G, or(A,B), bool) :- typeExpr(G, A, bool), typeExpr(G, B, bool).


typeExpr(G, app(id(X), Args), T) :- getValContexte(X,G, typeFun(A, T)), matching(G, Args, A).
typeExpr(G, app(fun(Ts, E), Args), T) :- 
        liste_to_type(Ts,TypeArgFun), 
        matching(G, Args, TypeArgFun),
        ajoutMultipleListe(Args, G, Gtemp),
        typeExpr(Gtemp, E, T).

typeExpr(G, app(app(X, Y), Args), T) :- 
        resType(G, Args, Tres),
        typeExpr(G, app(X,Y), typeFun(Tres , T)).

/*typeExpr(G, app(E, _), T) :- typeExpr(G, E, T).*/
typeExpr(G, app(E, Es), T) :- typeExpr(G, E, typeFun(Ts,T) ), matching(G,Ts,Es). 



typeExpr(G, abs(Args, E), typeFun(TypeL, Tsortie) ) :- 
    ajoutMultipleListe(Args, G, Gtemp),
    typeExpr(Gtemp, E, Tsortie),
    liste_to_type(Args,TypeL).
    

typeExpar(G, var(X), ref(T)) :- getValContexte(X,G,ref(T)).
typeExpar(G, E, T) :- typeExpr(G,E,T).



init([
    (true, bool),
    (false, bool),
    (not,typeFun([bool], bool)),
    (eq,typeFun([int, int], bool)),
    (lt,typeFun([int, int], bool)),
    (add,typeFun([int, int], int)),
    (sub,typeFun([int, int], int)),
    (mul,typeFun([int, int], int)),
    (div,typeFun([int, int], int))
]).


typeCheck(P,ok) :- typeProg(P).
typeCheck(_,ko).

exitCode(ok) :- halt(0).
exitCode(ko) :- halt(1).



