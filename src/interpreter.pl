% Program evaluation
eval_prog(t_prog(L),Env,FinalEnv) :-
    Env = [],
    eval_list(L,Env,FinalEnv).

% List evaluations
eval_list(t_list(D, C),Env,FinalEnv) :-
    eval_declaration(D,Env,FinalEnv), 
    eval_block_command(C,Env,FinalEnv);
    eval_declaration(D,Env,FinalEnv), 
    eval_command(C,Env,FinalEnv).

eval_list(t_list(D),Env,FinalEnv) :-
    eval_block_command(D,Env,FinalEnv);
    eval_command(D,Env,FinalEnv).

% Decleration evaluations
eval_declaration(t_decl(I, I2),Env,FinalEnv) :-
	eval_identifier(I,L),
    update(L, [], Env, FinalEnv),
    eval_declaration(I2,Env,FinalEnv).

eval_declaration(t_decl(I),Env,FinalEnv) :-
	eval_identifier(I, L),
    update(L, [], Env, FinalEnv).

% Command evaluations
eval_command(t_command(I),Env,FinalEnv) :-
    eval_print(I,Env,FinalEnv).

eval_command(t_command(I, I2),Env,FinalEnv) :-
	eval_identifier(I,L),
    eval_boolean(I2,Env,RBool),
    update(L, RBool, Env, FinalEnv);
    eval_identifier(I,L),
    eval_exp(I2,Env,RExp),
    update(L, RExp, Env, FinalEnv);
    eval_print(I,Env,FinalEnv), eval_command(I2,Env,FinalEnv).

eval_command(t_command(I, I2, I3),Env,FinalEnv) :-
	%eval_identifier(I,L),
    %eval_boolean(I2,Env, RBool),
    %update(L, RBool, Env, Env2),
    %eval_block_command(I3,Env2,FinalEnv);
    % alt
	eval_identifier(I,L),
    eval_exp(I2,Env,RExp),
    update(L, RExp, Env, Env2),
    eval_block_command(I3,Env2,FinalEnv);
    % alt
	eval_identifier(I,L),
    eval_boolean(I2,Env,RBool),
    update(L, RBool, Env, Env2),
    eval_command(I3,Env2,FinalEnv);
    % Alt
	eval_identifier(I,L),
    eval_exp(I2,Env,RExp), 
    update(L, RExp, Env, Env2),
    eval_command(I3,Env2,FinalEnv).

% Print evaluation shall use prolog 
% print predicate
eval_print(t_print(V),_,_) :-
    print(V).

% Block Command Evaluations
eval_block_command(t_block_cmnd(N),Env,FinalEnv) :-
	eval_while_command(N,Env,FinalEnv); 
    eval_if_command(N,Env,FinalEnv).

eval_block_command(t_block_cmnd(N, N2),Env,FinalEnv) :-
	eval_while_command(N,Env,FinalEnv), 
    eval_command(N2,Env,FinalEnv);
    eval_if_command(N,Env,FinalEnv), 
    eval_command(N2,Env,FinalEnv).
    
% While and conditional evaluation,
% futher breaking down parse tree to commands.
eval_while_command(t_while(X, Y),Env,FinalEnv) :-
    eval_boolean(X,Env,FinalEnv),
    eval_list(Y,Env,FinalEnv).

eval_if_command(t_if(X, Y, Z),Env,FinalEnv) :-
    eval_boolean(X,Env,FinalEnv),
    eval_list(Y,Env,FinalEnv),
    eval_list(Z,Env,FinalEnv).
    
% == BOOL eval =======================
eval_boolean(t_exp_eq(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    Result is (RExp1 =:= RExp2).

% && BOOL eval
eval_boolean(t_exp_and(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    Result is RExp1* RExp2.

% || BOOL eval
eval_boolean(t_exp_or(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    Result is RExp1+ RExp2.

% !(bool) BOOL eval
eval_boolean(t_exp_not(EXP, EXP2),Env, Result) :-
    eval_exp(EXP,Env, RExp1),
    eval_exp(EXP2,Env, RExp2),
    Result is NOT (RExp1 =:= RExp2).


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

eval_exp(t_expr(T),Env,Result) :- eval_term(T,Env,Result).

eval_exp(t_bool_true(_),_,true).
eval_exp(t_bool_false(_),_,false).

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

%Individual values
eval_factor(t_factor(N),_,Result) :- 
    eval_num(N,Result).
eval_factor(t_factor(I),Env,Result) :- 
    eval_identifier(I,L),
    lookup(L, Env, Result).                

eval_identifier(t_id(L),L).

eval_num(t_num(N),N).

% Predicate takes a 'x' value to find in Env
% once matched, returns the value of the
% identifier variable via tuple pair (Id, Val).
lookup(Search, [(Search,Val) | _], Val).
lookup(Search, [_|Env], Val) :- lookup(Search, Env, Val).

% Predicate finds value w/i Env1, updates the value in Env2
% If value doesn't exist in Env1, create it in Env2.
% Continue searching list if Env Heads match until value found
% is evident across X, Env1, Env2
update(Search, Val, [], [ (Search,Val) ]).
update(Search, Val, [(Search, _)|Env1], [(Search,Val)|Env1]).
update(Search, Val, [H|Env1], [H|Env2]) :- update(Search, Val, Env1, Env2).

interpreter(PTokens, FinalEnv) :-
	eval_prog(PTokens, Output, FinalEnv),
    print(Output), print(FinalEnv).