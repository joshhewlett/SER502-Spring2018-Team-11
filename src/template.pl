#!/usr/bin/env swipl

% What function to run when initialized
:- initialization(main, main).

% Loaded libraries
:- use_module(library(pio)).

main :-
	% Get the arguments passed from command line
	current_prolog_flag(argv,[Arg1|_]),

	% Create tokens from file passed through Argv
	phrase_from_file(tokens(Tokens), Arg1),

	% Get ASCII
	convert_to_ascii(Tokens, [], AsciiL),
	reverse_list_of_lists(AsciiL, [], CorrectAscii),

	% Print tokens
	write(CorrectAscii),

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
reverse_list_of_lists([H|T], ResultSoFar, Result) :-
	reverse_list(H, [], Word),
	atomics_to_string(Word, StringWord),
	string_concat("'", StringWord, AppendString1),
	string_concat(AppendString1, "'", AppendString2),
	reverse_list_of_lists(T, [AppendString2|ResultSoFar], Result).

% Reverse a list of lists
reverse_list([], Result, Result).
reverse_list([H|T], ResultSoFar, Result) :-
	reverse_list(T, [H|ResultSoFar], Result).