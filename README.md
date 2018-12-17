# Logic Programing - Assignment
A program that handles problems with polynomials and tests them in specific cases,
such as transforming a polynomial into a list of monomials,
simplifying a polynomial, summing two polynomials...

## About the Language
Programming Language Implemented: Prolog

Prolog is a logic programming language associated with artificial intelligence and computational linguistics.

Prolog has its roots in first-order logic, a formal logic, and unlike many other programming languages.
Prolog is intended primarily as a declarative programming language: the program logic is expressed in
terms of **relations**, represented as **facts** and **rules**.
A computation is initiated by running a query over these relations.

Learn more about [The Construction and Evaluation of a Prolog Techniques](https://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/)

In this case the program was tested with the software: SWI-Prolog 7.6.4 (via terminal)

## Made by

A program made for a college project by three students
of computer science from Faculty of Science of University of Porto


| Name            | University    | Country  |
| --------------- |:-------------:|--------: |
| Guilherme Amado | [FCUP][1]     | Portugal |
| João Vasco      | [FCUP][1]     | Portugal |
| José Pedro      | [FCUP][1]     | Portugal |

[1]: https://sigarra.up.pt/fcup/en/WEB_PAGE.INICIAL

## Some test cases (Queries)

```Prolog
  ?- poly2list(3*x^2+4*y-z+4*z^2, P).
  P = [3*x^2, 4*y, -z, 4*z^2].

  ? - addpoly(2*x+y, 4*y, P).
  P = 2*x+5*y.

  ? - scalepoly(2*y+5*x+6*x^2, 6, NP).
  NP = 36*x^2+30*x+12*y.

  ? - monomial(x^2).
  true.

  ? - polynomial(x^2-y+2*x).
  true.

  ? - polynomial(2*x^2).
  true.

```

Second part of the project is under construction.
