:- module(knight, [
    jump/1,
    on_board/1,
    move_knight/2,
    reach_in_3/2
]).

jump((X,Y)) :-
    member(X, [-2,-1,1,2]),
    member(Y, [-2,-1,1,2]),
    3 =:= abs(X) + abs(Y). 

on_board((X,Y)) :-
    X >= 1, X =< 8,
    Y >= 1, Y =< 8.

move_knight((X0,Y0), (X1,Y1)) :-
    jump((A,B)),
    X1 is X0+A,
    Y1 is Y0+B,
    on_board((X1,Y1)).

reach_in_3(Pos0, Pos3) :-
    move_knight(Pos0, Pos1),
    move_knight(Pos1, Pos2),
    move_knight(Pos2, Pos3).