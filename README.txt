1º Trabalho de Programaçao em Logica
Grupo: nº4
Membros:
  Guilherme Martins Amado   up201608528
  João Vasco Semblano       up201605518
  Jose Pedro Sousa          up201503443

> Ajuda para o desembolvimento do programa
  Alguns predicados dados nas aulas teoricas(slides)

> O programa é executado pelo terminal

> Software instalado --> swipl (SWI-Prolog)

> Como executar o programa:
   1. Abrir o terminal
   2. Executar o comando "swipl trabalho1"
   3. Executar casos teste

> Alguns casos teste:

   ? - monomial(x^2).
   true.

   ? - polynomial(x^2-y+2*x).
   true.

   ? - polynomial(2*x^2).
   true.

   ? - poly2list(2*x-y-z^4, L).
   L = [2*x, -y, -z^4].

   ? - simpoly(2*x+x, L).
   L = 0+3*x.

   ? - list2poly([2*x^2, x^2, y, -t], P).
   P = -t+y+x^2+2*x^2.

   ? - addpoly(2*x+y, 4*y, P).
   P = 2*x+5*y.

   ? - scalepoly(2*y+5*x+6*x^2, 6, NP).
   NP = 36*x^2+30*x+12*y.
