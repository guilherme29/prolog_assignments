
%:- [trabalho1].

plus--> [plus].
minus-->[minus].
mult--> [times].
raise-->[raised],[to].


polynomial(P+M) --> polynomial(P), plus, monomial(M), !.
polynomial(P-M) --> polynomial(P), minus, monomial(M), !.
polynomial(M)   --> monomial(M), !.
polynomial(-M)  --> minus, monomial(M), !.


monomial(V)     --> variable(V);
		    coefficient(V).
monomial(V^P)   --> variable(V), raise, power(P), !.
monomial(C*V)   --> coefficient(C), mult, variable(V), !.
monomial(C*V^P) --> coefficient(C), mult, variable(V), raise, power(P),!.


variable(V)-->[a], {V = a};
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
