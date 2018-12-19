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

% Read N diagnoses and retrieve them in a list
diagnosis(N, Diags) :-
    diagnosis(N, [], D),
    reverse(D, Diags).  % Assuming you want them listed in the order they were read

diagnosis(N, A, Diags) :-
    N > 0,
    rw_diag(Diag),      % read and write one diagnosis
    A1 = [Diag|A],
    N1 is N - 1,
    diagnosis(N1, A1, Diags).
diagnosis(0, A, A).

rw_diag(Diag) :-
    readln(Diag),
    write(Diag), nl.
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
add_opt([X|T]) :- remove_ele(with, T, NL).

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
	           [40],   {C = forty},!;
	           [50],   {C = fifty},!;
	           [60],   {C = sixty},!;
	           [70],   {C = seventy},!;
             [80],   {C = eighty},!;
	           [90],   {C = ninety}.

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

number(L, N) :- list_number(L, T), sumlist(T, N).

%[twenty, five] -----> [20, 5]
list_number([X], [Y]) :- unidade(X, [Y], []), !.
list_number([X|T], [Y|T2]) :- unidade(X, [Y], []), list_number(T, T2), !.

unidade(W, L, []) :- units(W, L, []).
unidade(W, L, []) :- til20(W, L, []).
unidade(W, L, []) :- tens(W, L, []).
