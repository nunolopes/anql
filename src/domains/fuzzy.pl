
:- module(fuzzy, []).
:- use_module('anql/sparql').

% ------------------
% Fuzzy functions
% ------------------

ardf_label('http://example.org/ardf#flabel').

%ardf:extract_label(literal(A), N) :- atom_codes(A, As), number_codes(N, As).
extract_label(numeric(decimal, A), N) :- !, number_codes(N, A).
extract_label(literal(type('http://www.w3.org/2001/XMLSchema#decimal',A)), N) :-
        atom_codes(A, As), number_codes(N, As).

different_labels(L1, L2):-
        L1 =\= L2.

% minimum of two fuzzy lables
infimum(L1, L2, Lr):- Lr is min(L1, L2).

% maximum of two fuzzy labels
supremum(L1, L2, Lr) :- Lr is max(L1, L2).
        
default(1).

lower_annotation(QueryAnnotation, DataAnnotation, ResultAnnotation):-
        ardf:infimum(QueryAnnotation, DataAnnotation, QueryAnnotation, ResultAnnotation).

% ---------------------
% SPARQL Annotation DCG
% ---------------------

annotation(D) -->
        sparql_grammar:decimal_string(A), {number_codes(D, A)}.

% ardf:less(A,B).


