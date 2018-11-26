/*
    Grupo: nº4
    Membros:
      Guilherme Martins Amado   up201608528
      João Vasco Semblano       up201605518
      Jose Pedro Sousa          up201503443

    Codigo baseado nos slides das aulas teoricas

*/

/* definitions */
pvars([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
pvar(X) :- pvars(V), member(X,V).

power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.
power(-X^Y):-pvar(X),integer(Y),Y>1,!.

coefficient(K):-number(K).

monomial(X):-pvar(X),!.
monomial(-X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(-N):-number(N),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.
monomial(K*X):-coefficient(K),pvar(X),!.

polynomial(M):-monomial(M),!.
polynomial(-M):-monomial(-M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.
polynomial(P-M):-monomial(-M),polynomial(P),!.

/* actual predicates */
%transforma um polinomio em uma lista de monomios
poly2list(P,L):-reverse(X,L),poly2listaux(P,X),!.
poly2listaux(M,[M]):- monomial(M),!.
poly2listaux(P+0,[P]).
poly2listaux(0+P,[P]).
poly2listaux(Y-X,[-X|Y1]):-poly2listaux(Y,Y1),!.
poly2listaux(Y+X,[X|Y1]):-poly2listaux(Y,Y1),!.

%simplifica um polinomio
simpoly(M,M2):-monomial(M),simmon(M,M2),!.
simpoly(P+0,P):-!.
simpoly(0+P,P):-monomial(P),!.
simpoly(P+M,P2+M3):-
    monparts(M,_,XExp),
    delmonomial(P,XExp,M2,P2),!,
    addmonomial(M,M2,M3).
simpoly(P+M,P2+M2):-simpoly(P,P2),simmon(M,M2),!.

simpoly(P-0,P):-!.
simpoly(0-P,-P):-monomial(-P),!.
simpoly(P-M,P2-M3):-
    monparts(M,_,XExp),
    delmonomial(P,XExp,M2,P2),!,
    submonomial(M,M2,M3).
simpoly(P-M,P2-M2):-simpoly(P,P2),simmon(M,M2),!.

%simplifica um monomio
simmon(1*P,P):-power(P),!.
simmon(0*_,0):-!.
simmon(M,M).

%divide um monomio em partes
monparts(X^N,1,X^N):-power(X^N),!.
monparts(K*P,K,P):-number(K),!.
monparts(K,K,indep):-number(K),!.
monparts(X,1,X):-pvar(X),!.

delmonomial(M,X,M,0):- monomial(M),monparts(M,_,X),!.
delmonomial(M+M2,X,M,M2):-
    monomial(M2),
    monomial(M),
    monparts(M,_,X), !.
delmonomial(P+M,X,M,P):- monomial(M),monparts(M,_,X),!.
delmonomial(P+M2,X,M,P2+M2):- delmonomial(P,X,M,P2).

%soma de dois monomios
addmonomial(K1,K2,K3):-
    number(K1),
    number(K2), !,
    K3 is K1+K2.
addmonomial(M1,M2,M3):-
    monparts(M1,K1,XExp),
    monparts(M2,K2,XExp),
    K3 is K1+K2,
    aux_addmonomial(K3,XExp,M3).

%subtracao de dois monomios
submonomial(K1,K2,K3):-
    number(K1),
    number(K2), !,
    K3 is K1-K2.
submonomial(M1,M2,M3):-
    monparts(M1,K1,XExp),
    monparts(M2,K2,XExp),
    K3 is K1-K2,
    aux_addmonomial(K3,XExp,M3).

aux_addmonomial(K,indep,K):-!.
aux_addmonomial(0,_,0):-!.
aux_addmonomial(1,XExp,XExp):-!.
aux_addmonomial(K,XExp,K*XExp).

%transforma uma lista num polinomio
list2poly([P],P):-monomial(P), !.
list2poly([P|L1], M+P):-list2poly(L1,M),!.

%simplifica uma lista de monomios
simpoly_list(V,X):- list2poly(V,Y), simpoly(Y,Z), poly2list(Z,X).

%soma de dois polinomios
addpoly(X,Y,Z):- poly2list(X+Y,L), simpoly_list(L,T), list2poly(T,Z), !.

scalelist(0, _, [0]).
scalelist(N, [X|[]], [R]):-
    number(N),
    scalemonomial(X,N,R),!.
scalelist(N, [X|T], [R|T2]):-
    number(N),
    scalemonomial(X,N,R),
    scalelist(N, T, T2),!.

scalemonomial(K1,K2,K3):-
    number(K1),number(K2),!,K3 is K1*K2.
scalemonomial(M1,M2,M3):-
    monomial(M1),
    monparts(M1,K1,XExp),
    number(M2),
    K3 is K1*M2,
    aux_addmonomial(K3,XExp,M3).

%multiplica um numero a um polinomio
scalepoly(P, N, NP):- poly2list(P, L), scalelist(N, L, NL), list2poly(NL, NP),!.
