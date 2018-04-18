program(t_prog(K)) --> ['Salutations', 'Xiangyu,'],
    list(K),
    ['Sincerely,', 'Ajay', 'Bansal'].

% This list is for the instance in which the program is declaring variables and
% executing commands on them.
list(t_list(D, C)) -->
    ['Would', 'you', 'mind', 'doing', 'the', 'following:'],
    declaration(D), block_command(C), ['Thank', 'you'], ['.'];
    ['Would', 'you', 'mind', 'doing', 'the', 'following:'],
    declaration(D), command(C), ['.'], ['Thank', 'you'], ['.'].

% This list is for the instance in which program is declaring commands with no
% declarations.
list(t_list(C)) -->
    ['Would', 'you', 'mind', 'doing', 'the', 'following:'],
    block_command(C), ['.'], ['Thank', 'you'], ['.'];
    ['Would', 'you', 'mind', 'doing', 'the', 'following:'],
    command(C), ['.'], ['Thank', 'you'], ['.'].

% Multi line declaration of variables.
declaration(t_decl(I, I2)) -->
    ['Create', 'the', 'variable'], identifier(I), ['.'], declaration(I2).

% Single declaration, TODO: Accomodate for multiple periods from declaration
% to list predicates.
declaration(t_decl(I)) -->
    ['Create', 'the', 'variable'], identifier(I), ['.'].

% Assignment of boolean expression expects a identifier var
% and a boolean value.
command(t_command(I, B)) -->
    ['Assign', 'the', 'boolean'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    boolean(B);
    ['Assign', 'the', 'integer'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    exp(B);
    command(I), ['.'], block_command(B);
    command(I), ['.'], command(B).

% Integer assignment expects a var identifier along with a
% numerical digit to bind with variable name.
%command(t_command(I, N)) --> ['Assign', 'the', 'integer'],
%    identifier(I),
%    ['to', 'the', 'value', 'of'],
%    number(N).

% Should it be the case you have a command followed by
% another command.
command(t_command(I, N)) --> command(I), ['.'], command(N);
    command(I), ['.'], block_command(N).

%Print command + assoc. print breakdown
% command(t_command(V)) --> ['Please', 'reply', 'with',
% 'the', 'value', 'of'], print(V),['.'].
%print(t_print(V)) -->
% identifier(V); boolean(V);number(N).



% Command can be comprised of a while loop, or conditional check.
% If this fails, we are doing a evaluated assignment.
block_command(t_block_cmnd(N, N2)) -->
    while_command(N), ['.'], command(N2);
    if_command(N), ['.'], command(N2).
% Case where a command proceeds after block command.
block_command(t_block_cmnd(N)) --> while_command(N); if_command(N).

% Break apart boolean and command, consume syntax to generate tree.
while_command(t_while(X, Y)) --> ['So', 'long', 'as'],
    boolean(X),
    ['please'],
    list(Y),
    ['your', 'iterations', 'are', 'appreciated'].

% Conditional check broken into Boolean evaluation and two commands.
% Each is concumsed and further passed down syntax tree.
if_command(t_if(X, Y, Z)) --> ['Should', 'it', 'be', 'the', 'case'],
    boolean(X),
    ['please'],
    list(Y),
    ['otherwise'],
    list(Z),
    ['that', 'is', 'all'].

% if, else if, else. TODO: handling multiple else if's
% if_command(t_if(X, Y, Z, A, B)) --> ['Should', 'it', 'be', 'the',
% 'case'],
%    boolean(X),
%    ['please'],
%    list(Y),
%    ['but', 'should', 'it', 'be', 'the', 'case'],
%    boolean(Z),
%    ['please'],
%    list(A),
%    ['otherwise'],
%    list(B),
%    ['that', 'is', 'all'].

% Booleans are comprised of evaluating single boolean statements or expressions
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

%Here begins the numerical comparators (<, <=, >, >=)
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ['LESS','THAN'],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ['GREATER','THAN'],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ['LESS','THAN','OR','EQUAL','TO'],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ['GREATER','THAN','OR','EQUAL','TO'],
%    exp(EXP2).

boolean(t_exp(EXP)) --> ['NOT'], exp(EXP).
boolean(t_exp(EXP)) --> exp(EXP).

% Precedence-defined arithmetic expressions that accomodate
% order of operations. Also breaks down boolean to TRUE/FALSE
% since boolean can consist of expressions, and need a base case.
exp(t_plus(T,E)) --> term(T),['+'],exp(E).
exp(t_minus(T,E)) --> term(T),['-'],exp(E).
exp(t_expr(T)) --> term(T).
exp(t_bool('TRUE')) --> ['TRUE'].
exp(t_bool('FALSE')) --> ['FALSE'].

%Precedence for arithmetic operations.
term(t_mult(F,T)) --> factor(F),['*'],term(T).
term(t_div(F,T)) --> factor(F),['/'],term(T).
term(t_term(F)) --> factor(F).

%Individual values
factor(t_factor(I)) --> identifier(I).
factor(t_factor(N)) --> num(N).

% Predicate defining variable names.
identifier(t_id(L)) --> [L], {
    atom_chars(L, Cs),
    length(Cs, N),
    length(Lowers,N),
    maplist(=(lower), Lowers),
    maplist(char_type, Cs, Lowers)}.

%Predicate defining numbers.
num(t_num(L)) --> [L], {
    atom_chars(L, Cs),
    length(Cs, N),
    length(Lowers,N),
    maplist(=(digit), Lowers),
    maplist(char_type, Cs, Lowers)}.

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
