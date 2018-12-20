:-[trabalho1].

nb_linkval(p1, 0).
nb_linkval(p2, 0).
nb_linkval(p3, 0).
nb_linkval(p4, 0).
nb_linkval(p5, 0).

%iniciaçao do programa
polyplay :-
    write("---Welcome---"), nl,
    polyplay2.

polyplay2 :-
    read(X),
    split_string(X, " ", "", L),
    string_list_to_atom(L, LA),
    option(LA, B), B is 1, nl,
    polyplay2.

%polyplay2 :- write("Error, something went bad!!"), polyplay2. %caso erro, imprime menssagem de erro

%definiçao das opçoes
shw_opt([X]) :- global_variavels(X, [G], []), nb_getval(G, Val), write(X),write("="),write(Val), nl.
shw_opt(L) :-
  atoms_to_numbers(NNL, [], P),
  atomic_list_concat(P, "", NP),
  write(NP).

gVar_is_ele([X|_], [Expr]) :- global_variavels(X, [V], []), nb_getval(V, Expr), term_string(E, Expr).

mul_opt([X|T]) :-
  remove_ele(by, T, NL),
  atoms_to_numbers(NL, [], P),
  atomic_list_concat(P, "", NP),
  atom_to_number(X, Num),
  term_string(K, NP),
  scalepoly(K, Num, NNP),
  write(NNP).

sim_opt(L) :-
  atoms_to_numbers(L, [], P),
  atomic_list_concat(P, "", NP),
  term_string(K, NP),
  simpoly(K, Poly),
  write(Poly).

add_opt([X|T]) :- remove_ele(with, T, L), global_variavels(X, [V], []), atoms_to_numbers(L, [], P), atomic_list_concat(P, "", NP), nb_setval(V, NP).

%conversao um atom para um numero
atom_to_number(A, N) :- unidade(A, [N], []), !.

%remove all intancies of that element X from the List and return a new list
remove_ele(_, [], []):-!.
remove_ele(X, [X|T], L):- remove_ele(X, T, L), !.
remove_ele(X, [H|T], [H|L]):- remove_ele(X, T, L ), !.

%converçao do texto num polinomio
%converter os atoms em numeros e simbulos e polos numa lista de atoms, depois concata-se tudo e fica numa exprressao

%atoms_to_numbers([X|[Y]], RPoly, P) :- square(X, Y, O), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- unidade(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- simbol(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- variable(X, L, []), append(L, RPoly, Poly2),reverse(Poly2, P), !.

atoms_to_numbers([X|T], Poly, P) :- unidade(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- simbol(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- variable(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.


% se tiver a palavra show,multiply, add, simplify como input entao vamos fazer a conversao de palavras para numeros
option([X|T], 1) :- X = show, shw_opt(T).
option([X|T], 1) :- X = multiply, mul_opt(T).
option([X|T], 1) :- X = add , add_opt(T).
option([X|T], 1) :- X = simplify, sim_opt(T).
option([X|_], 1) :- X = bye, write("Goodbye, friend"), nl, abort().
option([_|_], 0).


%converte uma string para uma lista de atoms
string_list_to_atom([X], [A]) :- string_to_atom(X, A),!.
string_list_to_atom([X|L1], [A|L2]) :- string_to_atom(X, A), string_list_to_atom(L1, L2),!.


number(L, N) :- list_number(L, T), sumlist(T, N).

list_number([X], [Y]) :- unidade(X, [Y], []), !.
list_number([X|T], [Y|T2]) :- unidade(X, [Y], []), list_number(T, T2), !.

unidade(W, L, []) :- units(W, L, []).
unidade(W, L, []) :- til20(W, L, []).
unidade(W, L, []) :- tens(W, L, []).

simbol(S) --> [+], {S = plus},!;
              [-], {S = minus},!;
              [*], {S = times},!;
              [^], {S = raised_to},!.


units(C) --> [0],    {C = zero},!;
             [1],    {C = one},!;
	           [2],    {C = two},!;
	           [3],    {C = three},!;
	           [4],    {C = four},!;
	           [5],    {C = five},!;
	           [6],    {C = six},!;
	           [7],    {C = seven},!;
	           [8],    {C = eight},!;
	           [9],    {C = nine}.

til20(C) --> [10],    {C = ten},!;
	           [11],    {C = eleven},!;
	           [12],    {C = twelve},!;
	           [13],    {C = thirteen},!;
	           [14],    {C = fourteen},!;
	           [15],    {C = fifteen},!;
	           [16],    {C = sixteen},!;
	           [17],    {C = seventeen},!;
	           [18],    {C = eighteen},!;
	           [19],    {C = nineteen}.

tens(C)  --> [20],   {C = twenty},!;
	           [30],   {C = thirty},!;
	           [40],   {C = fourty},!;
	           [50],   {C = fifty},!;
	           [60],   {C = sixty},!;
	           [70],   {C = seventy},!;
             [80],   {C = eighty},!;
	           [90],   {C = ninety}.

global_variavels(GV) --> [p1], {GV = 'P1'}, !;
                         [p2], {GV = 'P2'}, !;
                         [p3], {GV = 'P3'}, !;
                         [p4], {GV = 'P4'}, !;
                         [p5], {GV = 'P5'}.


variable(V) --> [a], {V = a},!;
		            [b], {V = b},!;
		            [c], {V = c},!;
		            [d], {V = d},!;
		            [e], {V = e},!;
		            [f], {V = f},!;
		            [g], {V = g},!;
		            [h], {V = h},!;
		            [i], {V = i},!;
		            [j], {V = j},!;
		            [k], {V = k},!;
		            [l], {V = l},!;
		            [m], {V = m},!;
		            [n], {V = n},!;
		            [o], {V = o},!;
		            [p], {V = p},!;
		            [q], {V = q},!;
		            [r], {V = r},!;
		            [s], {V = s},!;
		            [t], {V = t},!;
		            [u], {V = u},!;
		            [v], {V = v},!;
		            [w], {V = w},!;
		            [x], {V = x},!;
		            [y], {V = y},!;
		            [z], {V = z}.



/*
polynomial(P+M) --> polynomial(P), plus, monomial(M), !.
polynomial(P-M) --> polynomial(P), minus, monomial(M), !.
polynomial(M)   --> monomial(M), !.
polynomial(-M)  --> minus, monomial(M), !.

monomial(V)     --> variable(V);
coefficient(V).
monomial(V^P)   --> variable(V), raise, power(P), !.
monomial(C*V)   --> coefficient(C), mult, variable(V), !.
monomial(C*V)   --> number(C), mult, variable(V).
monomial(C*V^P) --> coefficient(C), mult, variable(V), raise, power(P),!.


plus  --> [plus].
minus --> [minus].
mult  --> [times].
raise --> [raised],[to].
*/
