/*
Exemplos e input que podemos ter:
  show two plus x squared
  > 2 + x^2

  multiply three by two plus x squared
  > 3*(2+x^2) ou seja 6+3*x^2

  simplify two plus two plus one times y
  > 2+2+1*y ou seja 4+y

  show two plus x squared as P1
  > P1=2+x^2

  multiply three by P1(2+x^2)
  > 3*P1 ou seja 6+2*x^2

  multiply three by P1 as P2
  > P2 = 3*P1

  add P1 with x raised to 3
  > P1 + x^3 ou seja 2+x^2+x^3

  show stored polynomials
  > P1 = 2+x^2
    P2 = 6+3*x^2

  forget P1 and show stored polynomials
  > P2 = 6 + 3*x^2
*/
:-[trabalho1].

%iniciamento do programa
polyplay :-
    write("Hello there...."), nl,
    read(X),
    split_string(X, " ", "", L),
    string_list_to_atom(L, LA),
    option(LA, B), B is 1, nl,
    polyplay.

polyplay :- write("Error, something is not right, you dumb fuck :^) "). %caso erro, imprime menssagem de erro

shw_opt(L) :- remove_ele(by, L, NL), atoms_to_numbers(NL, [], P), atomic_list_concat(P, "", NP), write(NP).

mul_opt([X|T]) :- remove_ele(by, T, NL), atoms_to_numbers(NL, [], P), atomic_list_concat(P, "", NP), atom_to_number(X, Num), term_string(K, NP), scalepoly(K, Num, NNP), write(NNP).

add_opt([X|T]) :- remove_ele(with, T, NL), global_variavels()
%conversao um atom para um numero
atom_to_number(A, N) :- units(A, [N], []), !.


%remove all intancies of that element X from the List and return a new list
remove_ele(_, [], []):-!.
remove_ele(X, [X|T], L):- remove_ele(X, T, L), !.
remove_ele(X, [H|T], [H|L]):- remove_ele(X, T, L ), !.

%converçao do texto num polinomio
%converter os atoms em numeros e simbulos e polos numa lista de atoms, depois concata-se tudo e fica numa exprressao ====> IDEIA
atoms_to_numbers([X], RPoly, P) :- units(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- simbol(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- variable(X, L, []), append(L, RPoly, Poly2),reverse(Poly2, P), !.

atoms_to_numbers([X|T], Poly, P) :- units(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- simbol(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- variable(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.

/*
%separaçao do primeiro elemento(show) com os restantes
sepSH([X|T], L) :- sep(T, L).
sep([X], [X]):-!.
sep([X|T], [X|T2]):- sep(T, T2),!.*/

% se tiver a palavra show,multiply, add, simplify como input entao vamos fazer a conversao de palavras para numeros
option([X|T], 1) :- X = show, shw_opt(T).
option([X|T], 1) :- X = multiply, mul_opt(T).
option([X|T], 1) :- X = add ,add_opt(T).
option([X|_], 1) :- X = simplify.
option([X|_], 0).


%converte uma string para uma lista de atoms
string_list_to_atom([X], [A]) :- string_to_atom(X, A),!.
string_list_to_atom([X|L1], [A|L2]) :- string_to_atom(X, A), string_list_to_atom(L1, L2),!.


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

zero(Z) --> [zero], {Z = 0}.

simbol(S) --> [+], {S = plus},!;
              [-], {S = minus},!;
              [*], {S = times},!;
              [^], {S = raised_to}.

units(C) --> [1],    {C = one},!;
	           [2],    {C = two},!;
	           [3],    {C = three},!;
	           [4],    {C = four},!;
	           [5],    {C = five},!;
	           [6],    {C = six},!;
	           [7],    {C = seven},!;
	           [8],    {C = eight},!;
	           [9],    {C = nine}.

til20(C) --> [ten],      {C = 10},!;
	           [eleven],   {C = 11},!;
	           [twelve],   {C = 12},!;
	           [thirteen], {C = 13},!;
	           [fourteen], {C = 14},!;
	           [fifteen],  {C = 15},!;
	           [sixteen],  {C = 16},!;
	           [seventeen],{C = 17},!;
	           [eighteen], {C = 18},!;
	           [nineteen], {C = 19}.

tens(C)  --> [twenty], {C = 20},!;
	           [thirty], {C = 30},!;
	           [forty],  {C = 40},!;
	           [fifty],  {C = 50},!;
	           [sixty],  {C = 60},!;
	           [seventy],{C = 70},!;
             [eighty], {C = 80},!;
	           [ninety], {C = 90}.

hundreds --> [hundred].
thousands --> [thousand].
millions --> [million];[millions].



/*
test --> (zero; units; tens).
test --> (units; tens), test2.
test2  --> hundreds; thousands; millions.
*/
global_variavels(GV) --> ['P1'], {GV = 'P1'}, !;
                         ['P2'], {GV = 'P2'}, !;
                         ['P3'], {GV = 'P3'}, !;
                         ['P4'], {GV = 'P4'}, !;
                         ['P5'], {GV = 'P5'}.


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

%coefficient(C) --> [C], {number(C)}.
power(P) --> [P], {number(P)}.

%predicados feitos pelo professor!!
number(C, L, [])-->units(C, L, []).
number(N, [Z], [])-->tens(T, [Y], []),units(C, [X], []),{Z is X+Y}.
