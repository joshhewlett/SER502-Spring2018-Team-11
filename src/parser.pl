program(t_prog(K)) --> ['Salutations', 'Xiangyu, '], 
    list(K),
    ['Sincerely,', 'Ajay', 'Bansal'].

list(t_list(D, C)) --> ['Would', 'you', 'mind', 'doing', 'the', 'following: '], 
    declaration(D), 
    [.],
    command(C),
    ['Thank you.'].

declaration(d(I)) --> ['Create', 'the', 'variable '], 
    identifier(I),
    ['.'].

declaration() --> declaration(_).

command(t_command(I, B)) --> ['Assign', 'the', 'boolean'], 
    identifier(I), 
    ['to', 'the', 'value', 'of'], 
    boolean(B).

command(t_command(I, N)) --> ['Assign', 'the', 'integer'], 
    identifier(I), 
    ['to', 'the', 'value', 'of'], 
    number(N).

command(t_command(C)) --> command(C).

% Break apart boolean and command, consume syntax to generate tree.
while_command(t_while(X, Y)) --> ['So', 'long', 'as'],
    boolean(X),
    ['please', 'do'],
    command(Y),
    ['thank', 'you', 'for', 'your', 'iterations'].				
    
% Conditional check broken into Boolean evaluation and two commands.
% Each is concumsed and further passed down syntax tree.
if_command(t_if(X, Y, Z)) --> ['Should', 'it', 'be', 'the', 'case'],
    boolean(X),
    [',','please','do'],
    command(Y),
    ['otherwise', 'do'],
    command(Z),
    ['that', 'is', 'all'].


boolean(t_boolean('TRUE')) --> ['TRUE'].
boolean(t_boolean('FALSE')) --> ['FALSE'].
boolean(t_exp(EXP, EXP2)) --> 
    exp(EXP), 
    ['EQUALS'], 
    exp(EXP2).
boolean(t_exp(EXP, EXP2)) --> 
    exp(EXP), 
    ['AND'], 
    exp(EXP2).
boolean(t_exp(EXP, EXP2)) --> 
    exp(EXP), 
    ['OR'], 
    exp(EXP2).
boolean(t_exp(EXP)) --> ['NOT'], exp(EXP).


exp(t_exp(N)) --> number(N).
%exp(t_bool(B)) --> boolean(B).
exp(t_exp(E)) --> 
    mult_exp(E);
    div_exp(E);
    add_exp(E);
    sub_exp(E).


mult_exp(t_mult(EXP, EXP2)) --> exp(EXP),['*'], exp(EXP2).
div_exp(t_div(EXP, EXP2)) --> exp(EXP), ['/'], exp(EXP2).
add_exp(t_add(EXP, EXP2)) --> exp(EXP), ['+'], exp(EXP2).
sub_exp(t_sub(EXP, EXP2)) --> exp(EXP), ['-'], exp(EXP2).

identifier(t_id(L)) --> letter(L).
identifier(t_id(I, L)) -->  identifier(I), letter(L).

letter(a) --> ['a'].
letter(b) --> ['b'].
letter(c) --> ['c'].
letter(d) --> ['d'].
letter(e) --> ['e'].
letter(f) --> ['f'].
letter(g) --> ['g'].
letter(h) --> ['h'].
letter(i) --> ['i'].
letter(j) --> ['j'].
letter(k) --> ['k'].
letter(l) --> ['l'].
letter(m) --> ['m'].
letter(n) --> ['n'].
letter(o) --> ['o'].
letter(p) --> ['p'].
letter(q) --> ['q'].
letter(r) --> ['r'].
letter(s) --> ['s'].
letter(t) --> ['t'].
letter(u) --> ['u'].
letter(v) --> ['v'].
letter(w) --> ['w'].
letter(x) --> ['x'].
letter(y) --> ['y'].
letter(z) --> ['z'].


number(t_num(0)) --> [0].
number(t_num(1)) --> [1].
number(t_num(2)) --> [2].
number(t_num(3)) --> [3].
number(t_num(4)) --> [4].
number(t_num(5)) --> [5].
number(t_num(6)) --> [6].
number(t_num(7)) --> [7].
number(t_num(8)) --> [8].
number(t_num(9)) --> [9].