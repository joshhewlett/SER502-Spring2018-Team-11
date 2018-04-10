#!/usr/bin/env swipl

% What function to run when initialized
:- initialization(main, main).
:- use_module(library(main)).

% Loaded libraries
:- use_module(library(pio)).

main(Argv) :-
	% Get the arguments passed from command line
	current_prolog_flag(argv,[Arg1|_]),

	% This prints out the arguments so it's easier to see what is going on
	format("Parsing file ~w \n",Arg1),
	% Parsing code goes here

	%atom_codes(Arg1, [H|_]),
	%atomic_list_concat([H|_], CharCode),
	phrase_from_file(tokens(Ls), Arg1),

	write('Hello!'),
	% Ends the execution
	halt.

tokens([]) --> call(eos), !.
tokens([Token|Tokens]) --> splitToken(Token), tokens(Tokens).

eos([], []).

splitToken([]) --> ( "\n" ; call(eos) ), !.
splitToken([]) --> ( " " ; call(eos) ), !.
splitToken([L|Ls]) --> [L], splitToken(Ls).
