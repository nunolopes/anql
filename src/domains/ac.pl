
:- module(ac, []).
:- use_module('anql/sparql').


% --------------------
% AC functions
% --------------------

ardf_label('http://example.org/ardf#aclabel').


default([[]]).


extract_label(literal(L), T):-
        atom_codes(L, LC),
        read_from_chars(LC, T0),
        sort(T0, T).



different_labels(L1, L2):-
        sort(L1, LS1),
        sort(L2, LS2),
        LS1 \== LS2.



% error:
% ?- ac:infimum([[a],[c]], [[a],[b]], X).
% false.


:- begin_tests(ac_infimum).

test(1):- ac:infimum([[nl]], [[-nl]], [[-nl]]).
test(2):- ac:infimum([[sk]], [[urq,-nl]], [[sk,urq,-nl]]).
test(3):- ac:infimum([[nl]], [[urq,sk,-nl]], [[sk,urq,-nl]]).
test(4):- ac:infimum([[-nl]], [[urq,sk,-nl]], [[sk,urq,-nl]]).
test(5):- ac:infimum([[nl,-sk]], [[sk]], [[nl,-sk]]).
test(6):- ac:infimum([[sk], [am]], [[am], [sk]], [[am, sk]]).

:- end_tests(ac_infimum).


infimum(A1, A2, AR):-
        findall(ART,(member(X, A1), member(Y, A2), append(X, Y, ART)), A3),
        simplify(A3, AR).

:- begin_tests(ac_supremum).

test(1):- ac:supremum([[-nl]], [[nl]], [[nl],[-nl]]).
test(2):- ac:supremum([[-sk]], [[nl]], [[nl],[-sk]]).
test(3):- ac:supremum([[nl,-sk]], [[sk]], [[sk], [nl,-sk]]).
test(4):- ac:supremum([[nl,-sk]], [[nl]], [[nl,-sk]]).
test(5):- ac:supremum([[nl, sk]], [[nl,-sk]], [[nl, sk],[nl,-sk]]).

:- end_tests(ac_supremum).



supremum(A1, A2, AR):-
        append(A1, A2, A3),
        simplify(A3, AR).

% simplify(+List, -Res)
% simplify an expression 
simplify(A, ANF) :-
        expand_annotations(A, [], Ae),
        sort_nested(Ae, [], As),
        normalise_rules(As, AN),
        sort_length(AN, AN2),
        redundant_rules(AN2, ANF).




% rule(emp, sk).
% rule(A, A).
rule('Karl Flannery', 'Managers' ).
rule('Sabrina Kirrane', 'Administrators' ).
rule('Sean O Malley', 'Administrators' ).
rule('John MacHale', 'TACIT Members' ).
rule('Brendan Walsh', 'TACIT Members' ).

:- begin_tests(ac_rules).

test(1):- ac:expand_annotations([[nl, 'Administrators']], [], [[nl, 'Sabrina Kirrane'], [nl, 'Sean O Malley'], [nl, 'Administrators']]).
test(2):- ac:expand_annotations([[nl, 'Administrators'], ['sk']], [], [[nl, 'Sabrina Kirrane'], [nl, 'Sean O Malley'], [nl, 'Administrators'], [sk]]).

:- end_tests(ac_rules).


%% expand the annotations in the list
expand_annotations([], R, R).
expand_annotations([R|LRs], Rit, RS):- 
        findall(Re, expand_list(R, Re), ReS),
        append(Rit, ReS, Rit1),
        expand_annotations(LRs, Rit1, RS).

expand_list([], []).
expand_list([U|LRs], [EU|RS]):- 
        rule(EU, U), 
        expand_list(LRs, RS).
expand_list([U|LRs], [U|RS]):-
        expand_list(LRs, RS).

%% sort the lists.
sort_nested([], R, RS) :- sort(R, RS).
sort_nested([R|LRs], Rit, RS):- 
        sort(R, Rs),
        sort_nested(LRs, [Rs|Rit], RS).


%% check for redundant rules.
redundant_rules([], []).
redundant_rules([R1|LRs], ANF):-
        is_subset_rules(LRs, R1), !,
        redundant_rules(LRs, ANF).
redundant_rules([R|LRs], [R|ANF]):-
%        \+ is_subset_rules(LRs, R1), !,
        redundant_rules(LRs, ANF).

% reached the end, not a subset of any rule
is_subset_rules([], _):- fail.
is_subset_rules([R1|_], R):-
        subset(R, R1), !.
is_subset_rules([_|LRs], R):-
        is_subset_rules(LRs, R).


% redundant_rules([], []).
% redundant_rules([R|Rs
 

%% apply conflict resolution
normalise_rules([],[]).
normalise_rules([R1|Rs], [RN|RNs]):-
        normalise_rule(R1, RN),
        normalise_rules(Rs, RNs).

% being sorted the positive literals are in the beginning.
normalise_rule([], []).
normalise_rule([L|Ls], [L|LNs]):-
        \+ memberchk(-L, Ls), !,    % negated atom not present
        normalise_rule(Ls, LNs).
normalise_rule([_|Ls], LNs):-
        normalise_rule(Ls, LNs).
        
% test_sort(I, O) :-
%          predsort(compare, I, O).
%          predsort(len_sort, I, O).

% len_sort(C,A,B):-
%          length(A, L1),
%          length(B, L2),
%          len_comp(L1, L2, C).
%

% len_comp(L1, L2, <) :- L1 < L2, !.
% len_comp(L1, L2, >) :- L1 > L2, !.
% len_comp(L1, L2, =) :- L1 = L2.
% 

sort_length(ILs, OLs) :-
                  maplist(add_length, ILs, TOLs),
                  sort(TOLs, STOLs),
                  maplist(remove_length, STOLs, OLs).
 
add_length(ILs, L-OLs) :-
                 length(ILs, L),
                 sort(ILs, OLs).

remove_length(_-Ls, Ls).

check_access_statement([], _) :- fail.
check_access_statement([AH|_], C) :-
                  check_denied(C, AH),
                  check_credential(AH, C), !.
check_access_statement([_|AT], C) :-
                  check_access_statement(AT, C).


check_denied([], _).
check_denied([CH|CT], A) :-
                  \+ memberchk(-CH, A),
                   check_denied(CT, A).

check_credential([], _).
check_credential([AH|AT], C) :-
                   memberchk(AH, C),
                   check_credential(AT, C).
                   
% TBD
lower_annotation([QueryAnnotation], DataAnnotation, ResultAnnotation) :-
       check_empty_query(QueryAnnotation),
       check_empty_statement(DataAnnotation),
       check_access_statement(DataAnnotation, QueryAnnotation),
       ResultAnnotation = [QueryAnnotation].

check_empty_query(X):- (nonvar(X), X = ['']) -> fail; true.

check_empty_statement([]):-fail.
check_empty_statement([AH|AT]):- (nonvar(AH), AH = []) -> check_empty_statement(AT); !.

% ---------------------
% SPARQL Annotation DCG
% ---------------------
 

annotation(L) -->
        "[", access_statement(L), "]".

access_statement([]) --> [].
access_statement([S|L]) -->
        "[", sparql_grammar:skip_ws, access_statement_contents(S), sparql_grammar:skip_ws, "]",
        access_statement(L).

access_statement_contents([Atom|T]) -->
        sparql_grammar:skip_ws, string(Codes), sparql_grammar:skip_ws, { atom_codes(Atom, Codes) },
        ("," -> access_statement_contents(T); { T=[] } ).

% access_statement_contents([Atom|T]) -->
%         string(Codes), { atom_codes(Atom, Codes) }, ",", !,
%         access_statement_contents(T).
% access_statement_contents([Atom]) -->
%         string(Codes), { atom_codes(Atom, Codes) }.
% 

string([H|T]) -->
        [H], { \+ memberchk(H, [44, 93]) },
        string(T).
string([]) --> "".

