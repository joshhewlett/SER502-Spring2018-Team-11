program(t_prog(K)) --> ['Salutations', 'Xiangyu, '],
    list(K),
    ['Sincerely,', 'Ajay', 'Bansal'].

% This list is for the instance in which the program is declaring variables and 
% executing commands on them.
list(t_list(D, C)) --> 
    ['Would', 'you', 'mind', 'doing', 'the', 'following: '],
    declaration(D), command(C), ['Thank', 'you.'];
    declaration(D), block_command(C), ['Thank', 'you.'].

% This list is for the instance in which program is declaring commands with no 
% declarations.
list(t_list(C)) --> 
    ['Would', 'you', 'mind', 'doing', 'the', 'following: '],
    command(C), ['Thank', 'you.']; 
    block_command(C), ['Thank', 'you.'].

% Single declaration, TODO: Accomodate for multiple periods from declaration
% to list predicates.
declaration(t_decl(I)) -->
    ['Create', 'the', 'variable'], identifier(I), ['.'].

% Multi line declaration of variables.
declaration(t_decl(I, I2)) -->     
    ['Create', 'the', 'variable'], identifier(I), ['.'],
    [','], declaration(I2).

% Assignment of boolean expression expects a identifier var
% and a boolean value.
command(t_command(I, B)) --> ['Assign', 'the', 'boolean'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    boolean(B).

% Integer assignment expects a var identifier along with a 
% numerical digit to bind with variable name.
command(t_command(I, N)) --> ['Assign', 'the', 'integer'],
    identifier(I),
    ['to', 'the', 'value', 'of'],
    number(N).

% Should it be the case you have a command followed by
% another command.
command(t_command(I, N)) --> command(I), ['.'], command(N);
    command(I), ['.'], block_command(N);.

% Command can be comprised of a while loop, or conditional check.
% If this fails, we are doing a evaluated assignment.
block_command(t_block_cmnd(N)) --> while_command(N); if_command(N).
% Case where a command proceeds after block command.
block_command(t_block_cmnd(N, N2)) --> while_command(N), command(N2); 
    if_command(N), command(N2).

% Break apart boolean and command, consume syntax to generate tree.
while_command(t_while(X, Y)) --> ['So', 'long', 'as'],
    boolean(X),
    ['please'],
    list(Y),
    ['thank', 'you', 'for', 'your', 'iterations'].

% Conditional check broken into Boolean evaluation and two commands.
% Each is concumsed and further passed down syntax tree.
if_command(t_if(X, Y, Z)) --> ['Should', 'it', 'be', 'the', 'case'],
    boolean(X),
    [',','please'],
    list(Y),
    ['otherwise'],
    list(Z),
    ['that', 'is', 'all.'].

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

% Predicates defining variable names.
% TODO: Accomodate for multi character strings.
identifier(t_id(L)) --> letter(L).
identifier(t_id(I, L)) -->  letter(L), identifier(I).

% TODO: Accomodate for multi digit values.
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
