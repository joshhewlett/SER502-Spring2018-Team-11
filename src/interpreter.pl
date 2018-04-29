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
    print(Val).

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
eval_num(t_num(N),M) :- string(N), atom_number(N,M).

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
