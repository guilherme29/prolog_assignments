Made by
A program made for a college project by three students of computer science from Faculty of Science of University of Porto

Name	              University	Country
Guilherme Amado     FCUP	      Portugal
José Pedro	        FCUP	      Portugal
João Vasco	        FCUP	      Portugal

to run:
swipl trabalho2.pl
?- polyplay.


Some test cases (Queries):
|: "show three plus four".
3+4
|: "show three times x raised_to four".
3*x^4
|: "add P1 with three times x raised_to four".
Added expression to variable P1.
|: "show P1".
P1 = 3*x^4
|: "bye".
See ya

Some Notes:
  -> The use of quotes is imperative for the program to work.
  -> We were able to store values and show them but not manipulate them.
  -> We fixed part of the first assignment(some errors that could not be overlooked for the realization of the second one).
  -> We couldn't make all numbers work.

sources for this work:
swi-prolog documentation
stackoverflow and google-fu in general
some tips and tricks from colleagues
