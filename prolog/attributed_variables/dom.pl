:- use_module(library(lists)).

intersection([], _, []).
intersection([X|Xs], Ys, [X|Zs]) :-
	member(X, Ys), !,
	intersection(Xs, Ys, Zs).
intersection([_|Xs], Ys, Zs) :-
	intersection(Xs, Ys, Zs).

verify_attributes(Var, Other, Goals) :-
	get_attr(Var, dom, DomA),
	(var(Other) ->
		get_attr(Other, dom, DomB),
		intersection(DomA, DomB, Dom),
		Dom \= [],
		put_attr(Other, dom, Dom),
		(Dom = [Value] ->
			Goals = (Other=Value) ;
			Goals = true
		)
	;
		member(Other, DomA),
		Goals = true
	).
