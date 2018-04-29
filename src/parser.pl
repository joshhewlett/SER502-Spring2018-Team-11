#!/usr/bin/env swipl

% What function to run when initialized
:- initialization(main, main).

% Loaded libraries
:- use_module(library(pio)).

main :-
    % TOKENIZER CALLS
	% Get the arguments passed from command line
	current_prolog_flag(argv,[Arg1|_]),

	% Create tokens from file passed through Argv
	phrase_from_file(tokens(Tokens), Arg1),

	% Get ASCII
	convert_to_ascii(Tokens, [], AsciiL),
	reverse_list_of_lists(AsciiL, [], CorrectAscii),
    write(CorrectAscii),
    write(' :-:-: '),

	% PARSER CALLS
    program(ParseTree, CorrectAscii, []),

    write(ParseTree),
    write(' :-:-: '),
    % Interpreter
    interpreter(ParseTree, FinalEnv),

    nl(),
    write(FinalEnv),

	% Ends the execution
	halt.

% ================================================ %
% ================ TOKENIZER ===================== %
% ================================================ %

% Create tokens from file
tokens([]) --> call(eos), !.
tokens([Token|Tokens]) --> splitToken(Token), tokens(Tokens).

% End of file
eos([], []).

% Split each token by a space
splitToken([]) --> ( "\n" ; call(eos) ), !.
splitToken([]) --> ( " " ; call(eos) ), !.
splitToken([L|Ls]) --> [L], splitToken(Ls).

% Convert a list of lists to ASCII
convert_to_ascii([], Result, Result).
convert_to_ascii([H|T], ResultSoFar, Result) :-
	get_ascii(H, [], Word),
	convert_to_ascii(T, [Word|ResultSoFar], Result).

% Convert a list to ASCII
get_ascii([], Result, Result).
get_ascii([H|T], ResultSoFar, Result) :-
	char_code(Char, H),
	get_ascii(T, [Char|ResultSoFar], Result).

% Reverse a list of lists
reverse_list_of_lists([], Result, Result).
reverse_list_of_lists([[]|T], ResultSoFar, Result) :-
	reverse_list_of_lists(T, ResultSoFar, Result).
reverse_list_of_lists([H|T], ResultSoFar, Result) :-
	reverse_list(H, [], Word),
	atomics_to_string(Word, StringWord),
	% string_concat(""", StringWord, AppendString1),
	% string_concat(AppendString1, """, AppendString2),
	reverse_list_of_lists(T, [StringWord|ResultSoFar], Result).

% Reverse a list of lists
reverse_list([], Result, Result).
reverse_list([H|T], ResultSoFar, Result) :-
	reverse_list(T, [H|ResultSoFar], Result).


% ================================================ %
% ================== PARSER ====================== %
% ================================================ %

% This identifiers the start, body, and end of our program.
program(t_prog(K)) --> ["Salutations", "Xiangyu,"],
    list(K),
    ["Sincerely,", "Ajay", "Bansal"].

% This list is for the instance in which the program is declaring variables and
% executing commands on them.
list(t_list(D, C)) -->
    ["Would", "you", "mind", "doing", "the", "following:"],
    declaration(D), block_command(C), ["Thank", "you"], ["."];
    ["Would", "you", "mind", "doing", "the", "following:"],
    declaration(D), command(C), ["."], ["Thank", "you"], ["."].

% This list is for the instance in which program is declaring commands with no
% declarations.
list(t_list(C)) -->
    ["Would", "you", "mind", "doing", "the", "following:"],
    block_command(C), ["."], ["Thank", "you"], ["."];
    ["Would", "you", "mind", "doing", "the", "following:"],
    command(C), ["."], ["Thank", "you"], ["."].

% Multi line declaration of variables.
declaration(t_decl(I, I2)) -->
    ["Create", "the", "variable"], identifier(I), ["."], declaration(I2).

% Single declaration, TODO: Accomodate for multiple periods from declaration
% to list predicates.
declaration(t_decl(I)) -->
    ["Create", "the", "variable"], identifier(I), ["."].

% This identifies the print command and the associated call. 
command(t_command(V)) --> ["Please", "reply", "with",
 "the", "value", "of"], print(V).


% Assignment and printing of boolean expression expects an
%identifier var and a boolean value.
command(t_command(I, B)) -->
    ["Assign", "the", "boolean"],
    identifier(I),
    ["to", "the", "value", "of"],
    boolean(B);
    ["Assign", "the", "integer"],
    identifier(I),
    ["to", "the", "value", "of"],
    exp(B);
    ["Please", "reply", "with", "the", "value", "of"],
    print(I), ["."], command(B).

% This assigns/prints a boolean expression if it is part
% of a block command.
command(t_command(C, D, E)) -->
    ["Assign", "the", "boolean"],
    identifier(C),
    ["to", "the", "value", "of"],
    boolean(D), ["."], block_command(E);
    ["Assign", "the", "integer"],
    identifier(C),
    ["to", "the", "value", "of"],
    exp(D), ["."], block_command(E);
    ["Assign", "the", "boolean"],
    identifier(C),
    ["to", "the", "value", "of"],
    boolean(D), ["."], command(E);
    ["Assign", "the", "integer"],
    identifier(C),
    ["to", "the", "value", "of"],
    exp(D), ["."], command(E).

% This identifies the variable to print.
print(t_print(V)) --> identifier(V).


% Command can be comprised of a while loop, or conditional check.
% If this fails, we are doing a evaluated assignment.
% Case where a command proceeds after block command.
block_command(t_block_cmnd(N)) --> while_command(N); if_command(N).
block_command(t_block_cmnd(N, N2)) -->
    while_command(N), ["."], command(N2);
    if_command(N), ["."], command(N2).

% Break apart boolean and command, consume syntax to generate tree.
while_command(t_while(X, Y)) --> ["So", "long", "as"],
    boolean(X),
    ["please"],
    list(Y),
    ["your", "iterations", "are", "appreciated"].

% Conditional check broken into Boolean evaluation and two commands.
% Each is concumsed and further passed down syntax tree.
if_command(t_if(X, Y, Z)) --> ["Should", "it", "be", "the", "case"],
    boolean(X),
    ["please"],
    list(Y),
    ["otherwise"],
    list(Z),
    ["that", "is", "all"].

% if, else if, else. TODO: handling multiple else if"s
% if_command(t_if(X, Y, Z, A, B)) --> ["Should", "it", "be", "the",
% "case"],
%    boolean(X),
%    ["please"],
%    list(Y),
%    ["but", "should", "it", "be", "the", "case"],
%    boolean(Z),
%    ["please"],
%    list(A),
%    ["otherwise"],
%    list(B),
%    ["that", "is", "all"].

% Booleans are comprised of evaluating single boolean statements or expressions
boolean(t_exp_eq(EXP, EXP2)) -->
    exp(EXP),
    ["EQUALS"],
    boolean(EXP2).

%Here begins the numerical comparators (<, <=, >, >=)
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ["LESS","THAN"],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ["GREATER","THAN"],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ["LESS","THAN","OR","EQUAL","TO"],
%    exp(EXP2).
%boolean(t_exp(EXP, EXP2)) -->
%    exp(EXP),
%    ["GREATER","THAN","OR","EQUAL","TO"],
%    exp(EXP2).

% Here are the remaining possible boolean operators
boolean(t_exp_not(EXP)) --> ["NOT"], boolean(EXP).
boolean(EXP) --> exp(EXP).

% Precedence-defined arithmetic expressions that accomodate
% order of operations. Also breaks down boolean to TRUE/FALSE
% since boolean can consist of expressions, and need a base case.
exp(t_plus(T,E)) --> term(T),["+"],exp(E).
exp(t_minus(T,E)) --> term(T),["-"],exp(E).
exp(T) --> term(T).
exp(t_bool_true("true")) --> ["TRUE"].
exp(t_bool_false("false")) --> ["FALSE"].

%Precedence for arithmetic operations.
term(t_mult(F,T)) --> factor(F),["*"],term(T).
term(t_div(F,T)) --> factor(F),["/"],term(T).
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



% =============================================
% ========== INTERPRETER ======================
% =============================================

% Program evaluation
eval_prog(t_prog(L),Env,FinalEnv) :-
    eval_list(L,Env,FinalEnv).

% List evaluations, comprised of either
% declarations and block commands, declarations and commands,
% or block commands proceeded by commands.
eval_list(t_list(D, C),Env,FinalEnv) :-
    eval_declaration(D,Env,Env2),
    eval_block_command(C,Env2,FinalEnv).

eval_list(t_list(D,C),Env,FinalEnv) :-
    eval_declaration(D,Env,Env2),
    eval_command(C,Env2,FinalEnv).

eval_list(t_list(D),Env,FinalEnv) :-
    eval_block_command(D,Env,FinalEnv);
    eval_command(D,Env,FinalEnv).

% Decleration evaluations, grab variable
% identifier, use literal to add/update environment,
% Recursive call to handle sequential variable declarations.
eval_declaration(t_decl(I, I2),Env,FinalEnv) :-
	eval_identifier(I,L),
    update(L, [], Env, Env2),
    eval_declaration(I2,Env2,FinalEnv).

% Declaration evaluation for singular declared variable.
eval_declaration(t_decl(I),Env,FinalEnv) :-
	eval_identifier(I, L),
    update(L, [], Env, FinalEnv).

% Command evaluation for instance of printing environment variable.
eval_command(t_command(I),Env,Env) :-
    eval_print(I,Env).

% Command evaluation instance for either
% boolean variable expressions, arithmetic variable expressions,
% or printing variable commands.
% Tail recursion handling of sequential command execution.
eval_command(t_command(I, I2),Env,FinalEnv) :-
	eval_identifier(I,L),
    eval_boolean(I2,Env,RBool),
    update(L, RBool, Env, FinalEnv);
    eval_identifier(I,L),
    eval_exp(I2,Env,RExp),
    update(L, RExp, Env, FinalEnv);
    eval_print(I,Env), eval_command(I2,Env,FinalEnv).

% Command evaluation for 4 different instances, each included
% tail recursive sequential command call.
eval_command(t_command(I, I2, I3),Env,FinalEnv) :-
    % Instance of boolean value assignment/operation,
    % proceeded by block command.
    eval_identifier(I,L),
    eval_boolean(I2,Env, RBool),
    update(L, RBool, Env, Env2),
    eval_block_command(I3,Env2,FinalEnv);
    % Instance of value assignment or arithmetic operation,
    % proceeded by block command.
	eval_identifier(I,L),
    eval_exp(I2,Env,RExp),
    update(L, RExp, Env, Env2),
    eval_block_command(I3,Env2,FinalEnv);
    % Instance of boolean assignment/operation,
    % proceeded by regular command.
	eval_identifier(I,L),
    eval_boolean(I2,Env,RBool),
    update(L, RBool, Env, Env2),
    eval_command(I3,Env2,FinalEnv);
    % Instance of value assignment or arithmetic operation,
    % proceeded by regular command.
	eval_identifier(I,L),
    eval_exp(I2,Env,RExp),
    update(L, RExp, Env, Env2),
    eval_command(I3,Env2,FinalEnv).

% Print evaluation, identify variable, grab environment value,
% print to screen.
eval_print(t_print(V),Env) :-
    eval_identifier(V, Identifier),
    lookup(Identifier, Env, Val),
    print(Identifier),
    print(=),
    print(Val),
    nl().

% Singular Block command execution comprised of 
% while loops, or conditional statements.
eval_block_command(t_block_cmnd(N),Env,FinalEnv) :-
	eval_while_command(N,Env,FinalEnv);
    eval_if_command(N,Env,FinalEnv).

% Sequential Block commands comprised of while loop then conditional,
% or conditional followewd by command.
eval_block_command(t_block_cmnd(N, N2),Env,FinalEnv) :-
	eval_while_command(N,Env,Env2),
    eval_command(N2,Env2,FinalEnv);
    eval_if_command(N,Env,Env2),
    eval_command(N2,Env2,FinalEnv).

% While and conditional evaluation,
% futher breaking down parse tree to commands.
eval_while_command(t_while(X, Y),Env,FinalEnv) :-
    while_looper(X,Y,Env,FinalEnv).

% While looper evaluations boolean conditional,
% provides local stack environment for declaration and commands,
% followed by recursive call to represent iterations of loop execution.
while_looper(X,Y,Env,FinalEnv) :-
    eval_boolean(X,Env,1),
    eval_list(Y,Env,Env2),
    while_looper(X,Y,Env2,FinalEnv).

% End statement for evaluating boolean conditional in while loop iterations.
while_looper(X,_,Env,Env) :-
    eval_boolean(X,Env,0).

% Conditional statement evaluation comprised of boolean evaluation for truth,
% conditional evaluation for false execution, both including a list for 
% block execution of code.
eval_if_command(t_if(X, Y, Z),Env,FinalEnv) :-
    eval_boolean(X,Env,1),
    eval_list(Y,Env,FinalEnv);
    eval_boolean(X,Env,0),
    eval_list(Z,Env,FinalEnv).

% == BOOL eval.
eval_boolean(t_exp_eq(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    bool_eq(RExp1,RExp2,Result).
% && BOOL eval
eval_boolean(t_exp_and(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    bool_and(RExp1,RExp2,Result).

% || BOOL eval
eval_boolean(t_exp_or(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    bool_or(RExp1,RExp2,Result).

% !(bool) BOOL eval
eval_boolean(t_exp_not(Bool),Env, Result) :-
    eval_boolean(Bool,Env,RExp1),
    bool_not(RExp1,Result).

% Single true/false boolean evaluation.
eval_boolean(t_exp(T),Env,Result):-
    eval_exp(T,Env,Result).

% Predicate rule for equivalent true/false logical comparison.
bool_eq(Boo,Boo,1).
bool_eq(Boo1,Boo2,0) :-
    Boo1 =\= Boo2.

% Predicate rule negated equivalent true/false logical comparison.
bool_neq(Boo1,Boo2,1) :-
    Boo1 =\= Boo2.
bool_neq(Boo1,Boo2,0) :-
    Boo1 == Boo2.

% Predicate rule for ! boolean operation.
bool_not(0,1).
bool_not(1,0).

% Predicate rules for possible AND combinations.
bool_and(1,1,1).
bool_and(0,_,0).
bool_and(_,0,0).

% Predicate rules for possible OR combinations.
bool_or(1,_,1).
bool_or(_,1,1).
bool_or(0,0,0).

% x + y
eval_exp(t_plus(T,E),Env,Result) :-
    eval_term(T,Env,RTerm),
    eval_exp(E,Env,RExp),
    Result is RTerm + RExp.
% x - y
eval_exp(t_minus(T,E),Env,Result) :-
    eval_term(T,Env,RTerm),
    eval_exp(E,Env,RExp),
    Result is RTerm - RExp.

% Return single element.
eval_exp(T,Env,Result) :- eval_term(T,Env,Result).

% Single true/false expression return.
eval_exp(t_bool_true(_),_,1).
eval_exp(t_bool_false(_),_,0).

%Precedence for arithmetic operations.
% x * y
eval_term(t_mult(F,T),Env,Result) :-
    eval_factor(F,Env,RFactor),
    eval_term(T,Env,RTerm),
    Result is RFactor * RTerm.
% x / y
eval_term(t_div(F,T),Env,Result) :-
    eval_factor(F,Env,RFactor),
    eval_term(T,Env,RTerm),
    Result is RFactor / RTerm.

eval_term(t_term(F),Env,Result) :- eval_factor(F,Env,Result).

% Individual value(s) return.
eval_factor(t_factor(N),_,Result) :-
    eval_num(N,Result).
eval_factor(t_factor(I),Env,Result) :-
    eval_identifier(I,L),
    lookup(L, Env, Result).

% Return variable identifier using uniformity.
eval_identifier(t_id(L),L).

% Return variable number using uniformity.
eval_num(t_num(N),N) :- integer(N).
eval_num(t_num(N),M) :- string(N), atom_number(N, M).

% Predicate takes a 'x' value to find in Env
% once matched, returns the value of the
% identifier variable via tuple pair (Id, Val).
lookup(Search, Env, Val) :- member((Search, Val), Env).

% Predicate finds value w/i Env1, updates the value in Env2
% If value doesn't exist in Env1, create it in Env2.
% Continue searching list if Env Heads match until value found
% is evident across X, Env1, Env2
update(Search, Val, Env, Env2) :-
    member((Search, _), Env),
    updateOld(Search,Val,Env,Env2).

update(Search,Val,Env,Env2) :-
    not(member((Search, _), Env)),
    append([(Search,Val)],Env,Env2).

updateOld(Search, Val, [(Search, _)|Env1], [(Search,Val)|Env1]).
updateOld(Search, Val, [H|Env1], [H|Env2]) :- updateOld(Search, Val, Env1, Env2).

% Value of interest is FinalEnv, empty list is ENV
% which undergoes continuous changes, and returned via
% FinalEnv.
interpreter(PTokens, FinalEnv) :-
	eval_prog(PTokens, [], FinalEnv).
