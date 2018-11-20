

pvars([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
pvar(X) :- pvars(V), member(X,V).


power(X):-pvar(X),!.
power(X^Y):-pvar(X),integer(Y),Y>1,!.

coefficient(K):-number(K).

monomial(X):-pvar(X),!.
monomial(N):-number(N),!.
monomial(X):-power(X),!.
monomial(K*X):-coefficient(K),power(X),!.
monomial(K*X):-coefficient(K),pvar(X),!.

polynomial(M):-monomial(M),!.
polynomial(P+M):-monomial(M),polynomial(P),!.
%polynomial(P-M):-monomial(-M),polynomial(P),!.

poly2list(X,[X]):-monomial(X),!.
poly2list(P+0,[P]).
poly2list(0+P,[P]):-monomial(P),!.
poly2list(Y+X,[X|Y1]):-poly2list(Y,Y1).


