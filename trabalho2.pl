
:- [trabalho1].

polynomial-->polynomial,plus,monomial;
	     polynomial,minus,monomial;
	     monomial;
	     minus,monomial.

monomial-->variable(X);
	   coefficient;
	   variable(X),raise,power;
	   coefficient,mult,variable(X),
	   coefficient,mult,variable(X),raise,power.

plus--> [plus].
minus-->[minus].
mult--> [times].
raise-->[raised],[to].

variable(X)-->[a], {X = a};
	      [b], {X = b};
	      [c], {X = c};
	      [d], {X = d};
	      [e], {X = e};
	      [f], {X = f};
	      [g], {X = g};
	      [h], {X = h};
	      [i], {X = i};
	      [j], {X = j};
	      [k], {X = k};
	      [l], {X = l};
	      [m], {X = m};
	      [n], {X = n};
	      [o], {X = o};
	      [p], {X = p};
	      [q], {X = q};
	      [r], {X = r};
	      [s], {X = s};
	      [t], {X = t};
	      [u], {X = u};
	      [v], {X = v};
	      [w], {X = w};
	      [x], {X = x};
	      [y], {X = y};
	      [z], {X = z};
	      
