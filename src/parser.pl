program(t_prog(K)) --> ['Salutations', 'Xiangyu, '],
    list(K),
    ['Sincerely,', 'Ajay', 'Bansal'].

list(t_list(D, C)) --> ['Would', 'you', 'mind', 'doing', 'the', 'following: '],
    declaration(D), [.], command(C);
    command(C),
    ['Thank', 'you.'].

declaration(t_decl(I)) -->
    ['Create', 'the', 'variable'],
    identifier(I),
    ['.'].
declaration(d(I)) --> declaration(I).


command(t_command(I, B)) --> ['Assign', 'the', 'boolean'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    boolean(B).

command(t_command(I, N)) --> ['Assign', 'the', 'integer'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    number(N).

command(t_command(N)) --> while_command(N).
command(t_command(N)) --> if_command(N).
command(t_command(C)) --> command(C).

% Break apart boolean and command, consume syntax to generate tree.
while_command(t_while(X, Y)) --> ['So', 'long', 'as'],
    boolean(X),
    ['please', 'do'],
    list(Y),
    ['thank', 'you', 'for', 'your', 'iterations'].

% Conditional check broken into Boolean evaluation and two commands.
% Each is concumsed and further passed down syntax tree.
if_command(t_if(X, Y, Z)) --> ['Should', 'it', 'be', 'the', 'case'],
    boolean(X),
    [',','please','do'],
    list(Y),
    ['otherwise', 'do'],
    list(Z),
    ['that', 'is', 'all'].


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
boolean(t_exp(EXP)) --> exp(EXP).


exp(plus(T,E)) --> term(T),['+'],exp(E).
exp(minus(T,E)) --> term(T),['-'],exp(E).
exp(expr(T)) --> term(T).
exp(t_bool('TRUE')) --> ['TRUE'].
exp(t_bool('FALSE')) --> ['FALSE'].

%Precedence
term(mult(F,T)) --> factor(F),['*'],term(T).
term(div(F,T)) --> factor(F),['/'],term(T).
term(term(F)) --> factor(F).

%Individual values
factor(factor(I)) --> identifier(I).
factor(factor(N)) --> number(N).

identifier(t_id(L)) --> letter(L).
identifier(t_id(I, L)) -->  letter(L), identifier(I).

num(t_num(L)) --> number(L).
num(t_num(I, L)) -->  number(I), num(L).


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


number(t_num(0)) --> ['0'].
number(t_num(1)) --> ['1'].
number(t_num(2)) --> ['2'].
number(t_num(3)) --> ['3'].
number(t_num(4)) --> ['4'].
number(t_num(5)) --> ['5'].
number(t_num(6)) --> ['6'].
number(t_num(7)) --> ['7'].
number(t_num(8)) --> ['8'].
number(t_num(9)) --> ['9'].
