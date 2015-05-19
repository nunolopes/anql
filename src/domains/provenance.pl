
:- module(provenance, []).
:- use_module('anql/sparql').

% --------------------
% Provenance functions
% --------------------

ardf_label('http://example.org/ardf#plabel').


default([[true]]).


extract_label(literal(L), T):-
        atom_codes(L, LC),
        read_from_chars(LC, T0),
        sort(T0, T).



different_labels(L1, L2):-
        sort(L1, LS1),
        sort(L2, LS2),
        LS1 \== LS2.



% error:
% ?- provenance:infimum([[a],[c]], [[a],[b]], X).
% false.


infimum([[false]], _, [[false]]) :- !.
infimum(_, [[false]], [[false]]) :- !.
infimum([[true]], A, A) :- !.
infimum(A, [[true]], A) :- !.
infimum(L1, L2, R) :-
        append_prov(L1, L2, [R_]), 
        simplify(R_, R).


append_prov([], _, []).
append_prov([L1|Ls], L2, [Lr|LRes]) :-
        append_prov0(L2, L1, Lr),
        append_prov(Ls, L2, LRes).

append_prov0([], _, []).
append_prov0([L2|L2s], L1, [R|LRes]) :-
        append(L1, L2, R_),
        sort(R_, R),            % remove duplicates
        append_prov0(L2s, L1, LRes).




% Creates the union of two labels.
supremum(_, [[true]], [[true]]) :- !.
supremum([[true]], _, [[true]]) :- !.
supremum(V1, V2, V) :-
        append(V1, V2, Vr1),
        simplify(Vr1, V).




% compare_length(-R, +A, +B)
% compares lists of atoms, where smaller length lists are always ordered first
compare_length(R, A, B):- length(A, La), length(B, Lb),
        (
         La == Lb -> compare(R, A, B);
         La < Lb -> R = <;
         R = >
        ).



% simplify(+List, -Res)
% simplify an expression in DNF
simplify(DNF, DNFS) :-
        predsort(compare_length, DNF, DNF_),
        check_subset(DNF_, DNFS).

% check_subset(+List, -Res)
% check if the first element of the list is a subset of any of the "following" elements
% if it is the first element remove the "later one" (more general) from the list 
check_subset([], []).
check_subset([Ls|LsT], [Ls|R]):-
        check_subset0(LsT, Ls, Lr),
        check_subset(Lr, R).

check_subset0([], _, []).
check_subset0([Ls1|LsT], Ls, Lr) :-
        subset(Ls, Ls1), !,
        check_subset0(LsT, Ls, Lr).
check_subset0([Ls1|LsT], Ls, [Ls1|Lr]) :-
        check_subset0(LsT, Ls, Lr).
        


% ---------------------
% SPARQL Annotation DCG
% ---------------------
 
% sparql_grammar:annotation(D) -->
%         sparql_grammar:decimal_string(A), {number_codes(D, A)}.



