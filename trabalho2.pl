/*-------- experiencias com prolog --------------
user_input :-
    repeat,
    read(Line),
		split_string(X, " ", "", L),
		string_list_to_atom(L, LA),

    (   Line = quit ->  write('Finished'), nl, !, true ; write("Continue"), fail  ).
*/

%atom_concat para substituir o P+M

/* ----- declaraçao de bibliotecas ------*/
%:-use_module(library(clpb)). % biblioteca de expressoes satesfaziveis (Expressoes Booleanas)



%iniciamento do programa
polyplay :-
    write("Hello there...."), nl,
    read(X),
    split_string(X, " ", "", L),
    string_list_to_atom(L, LA),
    option(LA, B),                  %escolhe a opçao (show, add, simplify, multiply,...)
    B is 1,                        %se tiver uma dessas opçoes continua o programa, senao falha e vai para o caso de erro
    sepSH(LA, NewLA), write(LA),             %separa da lista(LA) o nome da opçao e cria uma nova lista de atoms(NewLA)
    atoms_to_numbers(NewLA, [], P),    %converte uma lista de atoms em texto para uma lista de atoms em numeros e simbolos
    atomic_list_concat(P, "", LP), %concatena os elementos da lista P e transforma-os numa expressao(Polinomio)
    write(LP), nl.

polyplay :- write("Error, something is not right, you dumb fuck :^) "). %caso erro, imprime menssagem de erro

%converçao do texto num polinomio
%converter os atoms em numeros e simbulos e polos numa lista de atoms, depois concata-se tudo e fica numa exprressao ====> IDEIA
atoms_to_numbers([X], RPoly, P) :- units(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- simbol(X, L, []), append(L, RPoly, Poly2), reverse(Poly2, P),!.
atoms_to_numbers([X], RPoly, P) :- variable(X, L, []), append(L, RPoly, Poly2),reverse(Poly2, P), !.

atoms_to_numbers([X|T], Poly, P) :- units(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- simbol(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.
atoms_to_numbers([X|T], Poly, P) :- variable(X, L, []), append(L, Poly, Poly2), atoms_to_numbers(T, Poly2, P), !.

%separaçao do primeiro elemento(show) com os restantes
sepSH([X|T], L) :- sep(T, L).
sep([X], [X]):-!.
sep([X|T], [X|T2]):- sep(T, T2),!.

% se tiver a palavra show,multiply, add, simplify como input entao vamos fazer a conversao de palavras para numeros
option([X|_], 1) :- X = show.
option([X|_], 1) :- X = multiply.
option([X|_], 1) :- X = add.
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

simbol(S) --> [+], {S = plus};
              [-], {S = minus};
              [*], {S = times};
              [^], {S = raised_to}.

units(C) --> [1],    {C = one};
	           [2],    {C = two};
	           [3],    {C = three};
	           [4],    {C = four};
	           [5],    {C = five},!;
	           [6],    {C = six};
	           [7],    {C = seven};
	           [8],    {C = eight};
	           [9],    {C = nine}.

til20(C) --> [ten],      {C = 10};
	     [eleven],   {C = 11};
	     [twelve],   {C = 12};
	     [thirteen], {C = 13};
	     [fourteen], {C = 14};
	     [fifteen],  {C = 15};
	     [sixteen],  {C = 16};
	     [seventeen],{C = 17};
	     [eighteen], {C = 18};
	     [nineteen], {C = 19}.

tens(C)  --> [twenty], {C = 20};
	     [thirty], {C = 30};
	     [forty],  {C = 40};
	     [fifty],  {C = 50};
	     [sixty],  {C = 60};
	     [seventy],{C = 70};
	     [eighty], {C = 80};
	     [ninety], {C = 90}.

hundreds --> [hundred].
thousands --> [thousand].
millions --> [million];[millions].



/*
test --> (zero; units; tens).
test --> (units; tens), test2.
test2  --> hundreds; thousands; millions.
*/


variable(V) --> [a], {V = a};
		[b], {V = b};
		[c], {V = c};
		[d], {V = d};
		[e], {V = e};
		[f], {V = f};
		[g], {V = g};
		[h], {V = h};
		[i], {V = i};
		[j], {V = j};
		[k], {V = k};
		[l], {V = l};
		[m], {V = m};
		[n], {V = n};
		[o], {V = o};
		[p], {V = p};
		[q], {V = q};
		[r], {V = r};
		[s], {V = s};
		[t], {V = t};
		[u], {V = u};
		[v], {V = v};
		[w], {V = w};
		[x], {V = x};
		[y], {V = y};
		[z], {V = z}.

%coefficient(C) --> [C], {number(C)}.
power(P) --> [P], {number(P)}.

coefficient(C)-->[zero], {C = 0};
		 [one],  {C = 1};
		 [two],  {C = 2};
		 [three],{C = 3};
		 [four], {C = 4};
		 [five], {C = 5};
		 [six],  {C = 6};
		 [seven],{C = 7};
		 [eight],{C = 8};
		 [nine], {C = 9};
		 [ten],  {C = 10}.

list2number(LN, N) :- atomic_list_concat(LN, S), atom_number(S, N).

%predicados feitos pelo professor!!
number(C)-->units(C).
number(N)-->tens(T),units(C),{N is T+C}.
