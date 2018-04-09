% What function to run when initialized
:- initialization main.

main :-
	% Get the arguments passed from command line
	current_prolog_flag(argv,Argv),
	% This prints out the arguments so it's easier to see what is going on
	format("Parsing file ~w \n",Argv),
	% Parsing code goes here

	% Ends the execution
	halt.

