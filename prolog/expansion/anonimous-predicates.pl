% http://tau-prolog.org/sandbox/nLqTBn81

:- use_module(library(lists)).
:- op(1150, xfx, =>).
:- op(100, xfx, @).

:- dynamic(current_lambda/1).
current_lambda(0).

goal_expansion(V@(Head => Body), V = P) :-
	current_lambda(Id),
    retract(current_lambda(_)),
    succ(Id, Id_),
    asserta(current_lambda(Id_)),
    number_chars(Id, Chars),
    atom_chars(Atom, Chars),
    atom_concat('_lambda', Atom, P),
    F =.. [P|Head],
    assertz((F :- Body)).

% EXAMPLES
% F@([X,Y] => Y is X+1), maplist(F, [1,2,3,4,5,6], X).
% F@([X] => 0 is X mod 2), include(F, [1,2,3,4,5,6], X).
