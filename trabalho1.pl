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

simpoly(P,P):-
 aux_simpoly(P,P2),
 P==P2,!.
simpoly(P,P3):-
 aux_simpoly(P,P2),
 simpoly(P2,P3),!.

minus_op(M1,M2):- number(M1), M1 < 0, M2 is M1*(-1),!.
minus_op(-M,M):-!.
minus_op(-1*M,M):-!.
minus_op(M1,M1).

aux_simpoly(P-0,P):-!.
aux_simpoly(0-M,-M):-monomial(M),!.
aux_simpoly(P-M,M3):-
 monparts(M,_,XExp),
 delmonomial(P,XExp,M2,P2),
 P2 == 0,
 submonomial(M2,M,M3).
aux_simpoly(P-M,P2-R):-
 monparts(M,_,XExp),
 delmonomial(P,XExp,M2,P2),
 submonomial(M2,M,M3), minus_op(M3,R).


aux_simpoly(P-M,P2-M3):-
monparts(M,_,XExp),
delmonomial(P,XExp,M2,P2),!,
submonomial(M2,M,M3).
aux_simpoly(P-M,P2-M2):-aux_simpoly(P,P2),simmon(M,M2).


aux_simpoly(P+0,P):-!.
aux_simpoly(0+M,M):-monomial(M),!.

aux_simpoly(P+M,P2-R):-
   number(M),
   monparts(M,_,XExp),
   delmonomial(P,XExp,M2,P2),
   addmonomial(M,M2,M3),
   M3 < 0,
   minus_op(M3,R).

aux_simpoly(P+M,P2+M3):-
   monparts(M,_,XExp),
   delmonomial(P,XExp,M2,P2),!,
   addmonomial(M,M2,M3).
aux_simpoly(P+M,P2+M2):-aux_simpoly(P,P2),simmon(M,M2).
%Adicionar outro caso para se M2 for negativo

aux_simpoly(M,M2):-monomial(M), simmon(M,M2),!.

simmon(1*P,P):- power(P),!.
simmon(-1*P,-P):-power(P),!.
simmon(P*1,P):- power(P),!.
simmon(0-P,-P):-!.
simmon(0*_,0):-!.
simmon(_*0,0):-!.
%simmon(K1*K2,R):-number(K1), number(K2), R is K1*K2,!.
simmon(M,M).

mult(K1*K2,R):-number(K1), number(K2), R is K1*K2,!.

monparts(X^N,1,X^N):-power(X^N),!.
monparts(K*P,K,P):-number(K),!.
monparts(P*K,K,P):-number(K),!.
monparts(K,K,indep):-number(K),!.
monparts(X,1,X):-pvar(X),!.

expmonparts(X^N,N,X):-power(X^N),!.

delmonomial(M,X,M,0):-
 monomial(M),monparts(M,_,X),!.
delmonomial(M-M2,X,M,NM):-
 number(M2),monomial(M),monparts(M,_,X),
 NM is -M2,!.
delmonomial(P-M,X,NM,P):-
 number(M),NM is -M, monparts(NM,_,X),!.


delmonomial(M-K*M2,X,M,NK*M2):-
   monomial(M2),
   monomial(M),
   monparts(M,_,X), NK is -K,!.

delmonomial(P-M,X,-M,P):-
   monomial(M),
   monparts(M,_,X),!.


delmonomial(M-M2,X,M,-M2):-
   monomial(M2),
   monomial(M),
   monparts(M,_,X),!.

delmonomial(P-M,X,-M,P):-
   monomial(M),
   monparts(M,_,X),!.

delmonomial(P-M2,X,M,P2-M2):-
 delmonomial(P,X,M,P2).

delmonomial(M+M2,X,M,M2):-
 monomial(M2),monomial(M),monparts(M,_,X),!.
delmonomial(P+M,X,M,P):-
 monomial(M),monparts(M,_,X),!.
delmonomial(P+M2,X,M,P2+M2):-
 delmonomial(P,X,M,P2).

 addmonomial(-K1,K2,K3):-
   K3 is K2-K1.
 addmonomial(K1,K2,K3):-
  number(K1),number(K2),!,
  K3 is K1+K2.
 addmonomial(M1,M2,M3):-
    monparts(M1,K1,XExp),
    monparts(M2,K2,XExp),
    K3 is K1+K2,
    aux_addmonomial(K3,XExp,M3).

 aux_addmonomial(K,indep,K):-!.
 aux_addmonomial(0,_,0):-!.
 aux_addmonomial(1,XExp,XExp):-!.
 aux_addmonomial(K,XExp,K*XExp).

 submonomial(-K1,K2,K3):-
   number(K1),number(K2),!,
   K3 is -(K1+K2).
 submonomial(K1,K2,K3):-
  number(K1),number(K2),!,
  K3 is K1-K2.
 submonomial(M1,M2,M3):-
  monparts(M1,K1,XExp),
  monparts(M2,K2,XExp),
  K3 is K1-K2,
  aux_submonomial(K3,XExp,M3).


 aux_submonomial(K,indep,K):-!.
 aux_submonomial(0,_,0):-!.
 aux_submonomial(1,XExp,XExp):-!.
 aux_submonomial(K,XExp,K*XExp).

%transforma uma lista num polinomio
list2poly([P],P):-monomial(P), !.
list2poly([P|L1], M+P):-list2poly(L1,M),!.

%simplifica uma lista de monomios
%simpoly_list(V,X):- list2poly(V,Y), simpoly(Y,Z), poly2list(Z,X).
simpoly_list([],[]).
simpoly_list([X|L], L2) :- add_poly2list(X, L, 0, L3), simpoly_list(L3, L2),!.
simpoly_list([X|L], [R|L2]) :- simmon(X, X1), add_poly2list(X1, L, R, L3), simpoly_list(L3,L2), !.

add_poly2list(X, [], X, []).
add_poly2list(X, [Y|L1], R1, L2) :- addmonomial(X, Y, R), add_poly2list(R, L1, R1, L2), !.
add_poly2list(X, [Y|L1], R, [Y|L2]) :- add_poly2list(X,L1,R,L2),!.

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
