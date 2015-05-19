
:- module(temporal, []).

:- use_module(library(clpfd)).
:- use_module('anql/sparql').

% ------------------
% Temporal functions
% ------------------

ardf_label('http://example.org/ardf#tlabel').

extract_label(literal(L), T):-
        atom_codes(L, LC),
        read_from_chars(LC, T0),
        sort(T0, T).


different_labels(L1, L2):-
        sort(L1, LS1),
        sort(L2, LS2),
        LS1 \== LS2.

% dot(+I1, +I2, -I3)
% Creates the intersection of two temporal labels.

infimum(TL1, TL2, TLr):- infimum(TL1, TL2, [], TLr0), sort(TLr0, TLr).

infimum([], _, TLr, TLr).
infimum([V|Vs], V2, TLac, Vr):-
        itersection(V2, V, TLac, TLac1),
        infimum(Vs, V2, TLac1, Vr).

itersection([], _, TLac, TLac).
itersection([L|Ls], V, TLac, TLr):-
        (
         intersection(L, V, Int) ->
         NewAcc = [Int|TLac]
        ;
         NewAcc = TLac
        ),
         itersection(Ls, V, NewAcc, TLr).

% intersection(+I1, +I2, -I)
% I is the non-empty intersection of intervals I1 and I2.
intersection([A, B], [C, D], [IntInf, IntSup]):-
        X in A..B, Y in C..D,
        Int #= X, Int #= Y,
        fd_inf(Int, IntInf), 
        fd_sup(Int, IntSup).


% Creates the union of two temporal labels.
supremum(V1, V2, V) :-
        append(V1, V2, V3),
        build_domain_expr(Expr, V3),
        Z in Expr, % let CLPFD do the union
        fd_dom(Z, D),
        build_domain_expr(D, V0),
        sort(V0, V).
        

% build_domain_expr(?Expr, ?Label)
% Label is a temporal label (list of disjoint intervals)
% Expr is an expression of form (N1..M1) \/ ... \/ (Nk, Mk)
%build_domain_expr((A..B), [[A,B]]).
%build_domain_expr(A, [[A,A]]) :- ground(A).
%build_domain_expr(C \/ (A..B), [[A,B]|Rs]) :-
%     build_domain_expr(C, Rs).


% build_domain_expr((A..B), [[A,B]]) :- !.
% build_domain_expr(C \/ (A..B), [[A,B]|Rs]) :- !,
%     build_domain_expr(C, Rs).
% build_domain_expr(C \/ A, [[A,A]|Rs]) :- ground(A), !,
%     build_domain_expr(C, Rs).
% build_domain_expr(A, [[A,A]]) :- ground(A).

% build_domain_expr(C, [L]):- atomic(C), !,  % termination condition CLP -> List
%         clpfd_list(C, L).
% build_domain_expr(C, [L]):- nonvar(L), !, % termination condition List -> CLP
%         clpfd_list(C, L).
% build_domain_expr(A \/ B, [L|Rs]) :-
%         clpfd_list(B, L),
%         build_domain_expr(A, Rs).


% clpfd_list((A..B), [A,B]):- !.
% clpfd_list(A,      [A,A]).  % if List = [1,1] this is not used: doesn't matter for the CLP


build_domain_expr(CLP, LIST) :- var(CLP), !, build_domain_expr1(LIST, CLP).
build_domain_expr(CLP, LIST) :- build_domain_expr2(CLP, LIST).

% LIST -> CLP
build_domain_expr1([[A,B]], (A..B)) :- !.
build_domain_expr1([[A,B]|Rs], C \/ (A..B)) :-
     build_domain_expr1(Rs, C).

% CLP -> LIST
build_domain_expr2((A..B), O) :- !, O = [[A,B]].
build_domain_expr2(C \/ (A..B), [[A,B]|Rs]) :- !, 
     build_domain_expr2(C, Rs).
build_domain_expr2(C \/ A, [[A,A]|Rs]) :- !, 
     build_domain_expr2(C, Rs).
build_domain_expr2(A, [[A,A]]).



default([[inf, sup]]).



lower_annotation(QueryAnnotation, DataAnnotation, ResultAnnotation):-
        infimum(QueryAnnotation, DataAnnotation, Annotation),
        !, 
%        Annotation = QueryAnnotation, !,   % makes query 7 fail, check!
        ResultAnnotation = Annotation.

% ---------------------
% SPARQL Annotation DCG
% ---------------------

annotation(L) -->
        "[", interval_list(L), "]".

interval_list([[S,E]|L]) -->
        "[", sparql_grammar:integer(S), ",", sparql_grammar:integer(E), "]", !,
        interval_list(L).
interval_list([]) --> [].
% ardf:less(A,B).


% used in resolve function!!?!?
% we can eventually define these next 2 predicates automatically.
% % length(X,2), maplist(=(expression), X).
% use lowercase for the builtin functions here
built_in_function(beforeany(_,_)).
built_in_function(beforeall(_,_)).

% use lowercase for the builtin functions here
built_in_function(beforeany,    [expression,expression]).
built_in_function(beforeall,    [expression,expression]).

% 
sparql_runtime:op(beforeany(A,B),boolean(Result)) :- (map(A, before, any, any, B) -> Result = true; Result = false).
sparql_runtime:op(beforeall(A,B),boolean(Result)) :- (map(A, before, any, all, B) -> Result = true; Result = false).


map([], _, all, _, _) :- !.
map([], _, any, _, _) :- fail.
% check for all the intervals
map([L1|L1s], Relation, all, TypeL2, L2):- 
        map2(L2, Relation, TypeL2, L1), !,
        map(L1s, Relation, all, TypeL2, L2).
% if one interval succeds is enough
map([L1|L1s], Relation, any, TypeL2, L2):-
        (
         map2(L2, Relation, TypeL2, L1), !
        ;
         map(L1s, Relation, any, TypeL2, L2)
        ).


map2([], _, all, _) :- !.
map2([], _, any, _) :- fail.
map2([L2|L2s], Relation, all, L1) :-
        relation(Relation, L1, L2),
        map2(L2s, Relation, all, L1).
map2([L2|L2s], Relation, any, L1) :-
        (
         relation(Relation, L1, L2), !
        ;
         map2(L2s, Relation, any, L1)
        ).



%% Allen before
relation(before, [_A,B], [C,_D]) :- B < C, !.
relation(after,  [_A,B], [C,_D]) :- C < B, !.


:- begin_tests(relation_map).

test(1):- temporal:map([[1,2], [5,6]], before, any, any, [[3,4], [7,8]]).
test(2):- temporal:map([[1,2], [5,6]], before, any, all, [[3,4], [7,8]]).
test(3):- temporal:map([[1,2], [5,6]], before, all, any, [[3,4], [7,8]]).
test(3):- temporal:map([[1,2], [3,4]], before, all, any, [[5,6], [7,8]]).

test(5, [fail]):- temporal:map([[1,2], [5,6]], before, all, all, [[3,4], [7,8]]).

:- end_tests(relation_map).


% needed to register the domain functions
:- sparql_runtime:make_op_declarations.
