program(p(K)) --> [Salutations Xiangyu, ],k(K),[Sincerely, Ajay Bansal].
list(l(D, C)) --> [Would you mind doing the following: ], d(D), [.],c(C),[Thank you.]

declaration(d(I)) --> [Create the variable ], identifier(I),[.].
declaration() --> declaration(D).

command() --> command(C).
command() --> [Assign the boolean], identifier(I), [to the value of], boolean(B).
command() --> [Assign the integer], identifier(I), [to the value of], number(N).


boolean() --> ['TRUE'].
boolean() --> ['FALSE'].
boolean(e(EXP, EXP2)) --> exp(EXP), ['EQUALS'], exp(EXP2).
boolean(e(EXP, EXP2)) --> exp(EXP), ['AND'], exp(EXP2).
boolean(e(EXP, EXP2)) --> exp(EXP), ['OR'], exp(EXP2).
boolean(e(EXP)) --> ['NOT'], exp(EXP).

exp(mult(E)) --> mult_exp(E).
exp(div(E)) --> div_exp(E).
exp(add(E)) --> add_exp(E).
exp(sub(E)) --> sub_exp(E).
exp(num(N)) --> number(N).
exp(num(B)) --> boolean(B).

mult_exp(m(EXP, EXP2)) --> exp(EXP),[*], exp(EXP2).
div_exp(d(EXP, EXP2)) --> exp(EXP), [/], exp(EXP2).
add_exp(d(EXP, EXP2)) --> exp(EXP), [+], exp(EXP2).
sub_exp(s(EXP, EXP2)) --> exp(EXP), [-], exp(EXP2).

identifier(id(I, L)) -->  identifier(I), letter(L).

letter(a) --> [a].
letter(b) --> [b].
letter(c) --> [c].
letter(d) --> [d].
letter(e) --> [e].
letter(f) --> [f].
letter(g) --> [g].
letter(h) --> [h].
letter(i) --> [i].
letter(j) --> [j].
letter(k) --> [k].
letter(l) --> [l].
letter(m) --> [m].
letter(n) --> [n].
letter(o) --> [o].
letter(p) --> [p].
letter(q) --> [q].
letter(r) --> [r].
letter(s) --> [s].
letter(t) --> [t].
letter(u) --> [u].
letter(v) --> [v].
letter(w) --> [w].
letter(x) --> [x].
letter(y) --> [y].
letter(z) --> [z].


number(0) --> [0].
number(1) --> [1].
number(2) --> [2].
number(3) --> [3].
number(4) --> [4].
number(5) --> [5].
number(6) --> [6].
number(7) --> [7].
number(8) --> [8].
number(9) --> [9].

