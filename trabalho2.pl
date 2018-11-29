
%:- [trabalho1].

polynomial --> polynomial,plus,monomial;
	       polynomial,minus,monomial;
	       monomial;
	       minus,monomial.

monomial(V,C,P) --> variable(V);
		    coefficient(C);
		    variable(V), raise, power(P);
		    coefficient(C), mult, variable(V);
		    coefficient(C), mult, variable(V), raise, power(P).

plus--> [plus].
minus-->[minus].
mult--> [times].
raise-->[raised],[to].

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
	      
coefficient(C) --> [C], {number(C)}.
power(P) --> [P], {number(P)}.

