/*
%:- [trabalho1].

	text_to_string(texto, stringFinal) --> converte um dado texto numa so string
	term_string(termo, stringFinal) --> transforma um dado termo numa term_string
	split_string(StringInicial, CharQueQueremosSeparar,  PadingChars, StringFinal) --> converte uma string texto nums lista de strings (QUEREMOS ISTO)

*/

polyplay :- write("Hello there...."), nl, program.

program :- read(X), read_string(X, "\n", "\r", -1, S), split_string(S, " ", "", L), write(L), nl.

expr(X, R) :- polynomial(X,R).

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

units(C) --> [one],  {C = 1};
	     [two],  {C = 2};
	     [three],{C = 3};
	     [four], {C = 4};
	     [five], {C = 5};
	     [six],  {C = 6};
	     [seven],{C = 7};
	     [eight],{C = 8};
	     [nine], {C = 9}.

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
