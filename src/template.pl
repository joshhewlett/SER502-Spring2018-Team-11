#!/usr/bin/env swipl

% What function to run when initialized
:- initialization(main, main).

% Loaded libraries
:- use_module(library(pio)).

main :-
	% Get the arguments passed from command line
	current_prolog_flag(argv,[Arg1|_]),

	% Create tokens from file passed through Argv
	phrase_from_file(tokens(Ls), Arg1),

	% Print tokens
	write(Ls),
	% Ends the execution
	halt.

% Create tokens from file
tokens([]) --> call(eos), !.
tokens([Token|Tokens]) --> splitToken(Token), tokens(Tokens).

% End of file
eos([], []).

% Split each token by a space
splitToken([]) --> ( "\n" ; call(eos) ), !.
splitToken([]) --> ( " " ; call(eos) ), !.
splitToken([L|Ls]) --> [L], splitToken(Ls).
