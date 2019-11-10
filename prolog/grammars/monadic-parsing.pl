% http://www.tau-prolog.org/sandbox/jCmCyDQd

:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, chars).
:- op(700, xfy, >>=).

white --> [' '].
whites --> white, !, whites.
whites --> [].

digit(X) --> [X], {char_code(X, C), C >= 48, C =< 57}.
digits([X|Xs]) --> digit(X), !, digits(Xs).
digits([]) --> [].

int(N) --> digits([X|Xs]), whites, {number_chars(N, [X|Xs])}.
n_ints(0, []) --> !, [].
n_ints(N, [X|Xs]) --> int(X), {succ(M, N)}, n_ints(M, Xs).
len(Xs, L) --> {length(Xs, L)}, int(L).

run_parser(X >>= Y) -->
    call(X),
    {X =.. [_|Args], last(Args, Arg)},
    run_parser(Arg, Y).
run_parser(X) -->
    {X \= (_ >>= _)},
    call(X).
run_parser(Arg, X >>= Y) -->
    {X =.. [P|Args], X_ =.. [P,Arg|Args]},
    call(X_),
    {last(Args, Arg_)},
    run_parser(Arg_, Y).
run_parser(Arg, X) -->
    {X \= (_ >>= _), X =.. [P|Args], X_ =.. [P,Arg|Args]},
    call(X_).

% EXAMPLE
% run_parser(int(N) >>= n_ints(X) >>= len(L), "3 0 1 2 3", Rest).
% phrase((int(N), n_ints(N, X), len(X, L)), "3 0 1 2 3", Rest).
