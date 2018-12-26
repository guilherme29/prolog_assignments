# Polynomial Calculator.
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

A program made for a college assignment by three students
of computer science from Faculty of Science of University of Porto


| Name            | University    | Country  |    Linkdin    |
| --------------- |:-------------:|:-------: | -------------:|
| José Pedro      | [FCUP][1]     | Portugal |  [Profile][2] |
| Guilherme Amado | [FCUP][1]     | Portugal |  [Profile][3] |
| João Vasco      | [FCUP][1]     | Portugal |  [Profile][4] |

[1]: https://sigarra.up.pt/fcup/en/WEB_PAGE.INICIAL
[2]: https://www.linkedin.com/in/jose-pedro-sousa-71328612a/
[3]:
[4]:
## First part of the assignment
  Implementation of the "functions" used to calculate/simplify the input polynomials by the user.
  For more information check [here the statement][5]

[5]: https://github.com/guilherme29/prolog_assignments/blob/master/assignment1.pdf

### Some test cases (Queries)

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

## Second part of the assignment
  Implementation of a Definite Clause Grammar (DCG) used to translate an extended text into polynomials and
  to perform certain operations, such as adding, multiplying, or simplifying polynomials.

  We tried to get this calculator to decimal numbers, but we still can not get this "feature".
  We are working to get this tool as soon as possible.

  For more information check [here the statement][6]

[6]: https://github.com/guilherme29/prolog_assignments/blob/master/assignment2.pdf

### Some test cases (Queries)
  To run the program you need to go to a "secondary shell", by typing the next comand:
  ```Prolog
  ? - polyplay.
  ```

  and press ENTER key.

#### Comands

|                       Comand                        |             Output              |                    Meaning                       |
| --------------------------------------------------- | :------------------------------:|------------------------------------------------: |
| "show three plus x"                                 | 3+x                             | it is more like an print                         |
| "multiply three by x plus y"                        | 3*x+3*y                         | multiply the hole expression by a given number   |
| "add P1 with five times z"                          | Added expression to variable P1 | save an given expression into a variable P1      |
| "simplify three plus one plus x plus three times x" | 5+4*x                           | simplify an given expression                     |
| "bye"                                               | See ya                          | exit the "secondary shell"                       |   

```
  ? -
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
```

### Notes

  The work has already been delivered to the teacher of the discipline but the project is not over yet.

  This work is still under development and anyone who wants to help to improve, it is welcome to join, but for it you must contact
  one of the administrators (one of the students in the table indicated above).
