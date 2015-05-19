
:- module(compound, [ ardf_label/1,
                      default/1,
                      extract_label/2,
                      different_labels/2,
                      infimum/3,
                      supremum/3 %,
%                      annotation/3 ,
%                      built_in_function/1,
%                      built_in_function/2,
%                      sparql_runtime:op/2
                    ]).

:- use_module(aRDF).
% :- use_module(trdf).
% :- use_module(provenance).



init(A, B):-
        use_module(A),
        use_module(B),
        nb_setval(domains, (A,B)).

domains(A,B):- nb_getval(domains, (A,B)).
first(A):- nb_getval(domains, (A,_)).
second(B):- nb_getval(domains, (_,B)).



% ---------------------------------
% Temporal and provenance functions
% ---------------------------------

% predicates that need to be defined for aRDF:
% * ardf_label/1
% * default/1
% * extract_label/2
% * different_labels/2
% * infimum/3
% * supremum/3

% predicates that need to be defined for AnQL:
% * annotation/3  (DCG, -->)
% * built_in_function/1
% * built_in_function/2
% * sparql_runtime:op/2

ardf_label('http://example.org/ardf#label').


default([(Dt, Dp)]) :- domains(A,B), A:default(Dt), B:default(Dp).


extract_label(literal(L), T):-
	atom_codes(L, LC),
	read_from_chars(LC, T0),
	sort(T0, T).


different_labels(L1, L2):-
        sort(L1, LS1),
        sort(L2, LS2),
        LS1 \== LS2.


% % pointwise combination
% infimum((T1, P1), (T2, P2), (T, P)) :- domains(A,B), A:infimum(T1, T2, T), B:infimum(P1, P2, P).
% supremum((T1, P1), (T2, P2), (T, P)) :- domains(A,B), A:supremum(T1, T2, T), B:supremum(P1, P2, P).


infimum(A, B, Res) :-
        domains(DA,DB), 
        findall((T,P),
                (
                 member((At,Ap), A),
                 member((Bt, Bp), B),
                 DA:infimum(At, Bt, T),
                 DB:infimum(Ap, Bp,P)
                ),
                R),
        normalise(R, Res).


supremum(A, B, Res) :-
        append(A, B, R),
        normalise(R, Res).




% reduce(+PairsList, -Res)
% return a list with distint elements of L1. If there are more element with the same annotation for the
% first element compute the supremum of L2.
reduce(A,B):-
        reduce0(A, R),
        R == A, !, B = R.
reduce(A, C) :-
        reduce0(A,B),
        reduce(B, C).

reduce0([], []).
reduce0([([], _)|PairsList], Res):- !,
        reduce0(PairsList, Res).
reduce0([(_, [['false']])|PairsList], Res):- !,
        reduce0(PairsList, Res).
reduce0([Pair|PairsList], [NewPair|Res]):-
        reduce0(PairsList, Pair, NewPair, NewPairsList),
        reduce0(NewPairsList, Res).

reduce0([], Res, Res, []).
reduce0([(Elem1, Elem2)|Rest], (ElemIt1, ElemIt2), NewElem, Res):-
        Elem1 == ElemIt1, !,
        second(D),
        % domains(A,D),
        % A:supremum(Elem1, ElemIt1, Elem1), !,
        D:supremum(Elem2, ElemIt2, EP),
        reduce0(Rest, (Elem1, EP), NewElem, Res).
reduce0([(Elem1, Elem2)|Rest], (ElemIt1, ElemIt2), NewElem, Res):-
        Elem2 == ElemIt2, !,
        first(D),
        % domains(A,D),
        % A:supremum(Elem1, ElemIt1, Elem1), !,
        D:supremum(Elem1, ElemIt1, EP),
        reduce0(Rest, (EP, Elem2), NewElem, Res).
reduce0([(Elem1, Elem2)|Rest], (ElemIt1, ElemIt2), NewElem, Res):-
        domains(A,D),
        A:supremum(Elem1, ElemIt1, ElemIt1), 
        D:supremum(Elem2, ElemIt2, ElemIt2), !,
        reduce0(Rest, (ElemIt1, ElemIt2), NewElem, Res).
reduce0([(Elem1, Elem2)|Rest], (ElemIt1, ElemIt2), NewElem, Res):-
        domains(A,D),
        A:supremum(Elem1, ElemIt1, Elem1), 
        D:supremum(Elem2, ElemIt2, Elem2), !,
        reduce0(Rest, (Elem1, Elem2), NewElem, Res).
reduce0([E|Rest], Elem, NewElem, [E|Res]):-
        reduce0(Rest, Elem, NewElem, Res).




% saturate
% saturate_annotation(+Ints, -IntSat)
% expand interval annotations (used for complex domains)
saturate_annotation(Ints, IntSat) :-
        domains(DA,DB),
        findall((Rt, Rp),
                (
                 member((At, Ap), Ints),
                 member((Bt, Bp), Ints),
                 (
                  DA:supremum(At, Bt, Rt),
                  DB:infimum(Ap, Bp, Rp); %, Rp \= [[false]];
                  DA:infimum(At, Bt, Rt), % Rt \= [[]],
                  DB:supremum(Ap, Bp, Rp)
                 )
                ), Is),
        sort(Is, IntSat).



normalise(Ints, NormInts):-
        saturate_annotation(Ints, SatInts),
        reduce(SatInts, NormInts).
