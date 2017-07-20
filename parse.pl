% swi prolog
:- dynamic defined/2.

eval(X,O) :- defined(X,A), eval(A,O).
eval([X|T],O) :- defined(X,A), eval([A|T],O).

eval(X,X) :- number(X); X = t ; X = [].

eval([quote,X],X).
eval([lambda|X],[lambda|X]).
eval([define,X,Y],t) :- \+ defined(X,_), asserta(defined(X,Y)).

eval([cond],_) :- !, fail.
eval([cond,[H,A]|_],Z) :- eval(H,O), \+ O = [],!, eval(A,Z).
eval([cond,_|T],Z) :- eval([cond|T],Z),!.

eval([F|A],X) :- eval_list(A,Ae), apply(F,Ae,X),!.

eval_list([],[]).
eval_list([H|T],[Ho|To]) :- eval(H,Ho), eval_list(T,To).

apply(add,[X,Y],Z) :- Z is X + Y.
apply(add,[X,Y|T],Z) :- I is X + Y, apply(add,[I|T],Z).

apply(mul,[X,Y],Z) :- Z is X * Y.
apply(mul,[X,Y|T],Z) :- I is X * Y, apply(mul,[I|T],Z).

apply(sub,[X,Y],Z) :- Z is X - Y.
apply(div,[X,Y],Z) :- Z is X / Y.

apply(mod,[X,Y],Z) :- Z is X mod Y.
apply(pow,[X,Y],Z) :- Z is X ** Y.

apply(eq,[X,Y],t) :- X = Y.
apply(eq,[X,Y],[]) :- \+ X = Y.

apply(atom,[X],t) :- atom(X); X = [].
apply(atom,[[_|_]],[]).

apply(cons,[X|[Y]],[X|Y]).
apply(car,[[X|_]],X).
apply(cdr,[[_|T]],T).

apply([lambda,[],E],[],O) :- eval(E,O).
apply([lambda,[A|Ta],E],[L|Tl],O) :- subst(A,L,E,E2), apply([lambda,Ta,E2],Tl,O).

subst(_,_,[],[]).
subst(A,B,[A|T],[B|L]) :- subst(A,B,T,L).
subst(A,B,[H|T],[H|L]) :- subst(A,B,T,L).
subst(A,B,A,B).
subst(_,_,X,X).

% parser



parse(X,S) :- phrase(program(S),X),!.

program(H) --> ows, sexp(H), ows.

ws --> [X], {code_type(X, white)}, ows.
ows --> ws ; [].

sexp([]) --> "(", ows, ")".
sexp(S) --> "(", sexp_head(H), sexp_tail(H,S), ows, ")".

sexp_head(H) --> sexp(H); atom(H).
atom(A) --> number(A); identifier(A).

% takes head, returns full list
sexp_tail(H, [H|T]) --> ws, sexp_head(I), sexp_tail(I, T).
sexp_tail(H, [H]) --> [].


number(N) --> digit(D0), digits(D), { number_codes(N, [D0|D]) },!.
digits([D|T]) --> ("_" -> !; []),digit(D), digits(T).
digits(O) --> ".",digit(D0),!, {append(".",[D0|T],O)}, digits(T).
digits([]) --> [].
digit(D) --> [D], {code_type(D, digit)},!.

identifier(A) -->  csym(C),csyms(N), {string_to_atom([C|N],A)},!. 
csyms([H|T]) --> csym_(H), csyms(T).
csyms([]) --> [].
csym(C) --> [C], {code_type(C, csymf)}.
csym_(C) --> [C], {code_type(C, csym)}.

run(P,X) :- parse(P, O), eval(O,X).

% use ``'s not ""s for 'reasons'
% ?- parse(`(add 1 2 3 4)`,P), eval(P, X).
% P = [add, 1, 2, 3, 4],
% X = 10.

