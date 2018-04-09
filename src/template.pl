:- initialization main.

main :-
	current_prolog_flag(argv,Argv),
	format("Parsing file ~w \n",Argv),
	halt.

