:- module(test_anql, [test/3, check_result/2]).
:- use_module('ardf/aRDF').
:- use_module('anql/sparql').

% ----------------------------------
% Testcases  -- auxiliary predicates
% ----------------------------------

% test queries
test(Query, File, Res) :- 
        sparql:read_file(File, Query, Input, Q1),
        ardf:run(Input),
        sparql:sparql_query(Q1, Res, []).


check_result(Query, File) :-
        bagof(X, test(Query, File, X), L), sort(L, L1),
        sparql:get_result(File, Query, L1).


