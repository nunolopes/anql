:- use_module('ardf/aRDF').
:- use_module('tests/test_anql').
:- initialization(run_tests).



% -------------------------
% Testcases -- Temporal RDF
% -------------------------


% test datasets
:- begin_tests(anql_AC).

test(q1):-
        test_anql:check_result(q1, 'tests/queries/ac-anql.pl').
test(q2):-
        test_anql:check_result(q2, 'tests/queries/ac-anql.pl').
test(q3, [fail]):-
        test_anql:check_result(q3, 'tests/queries/ac-anql.pl').
% % test fails!?!?
% test(q4):-
%         test_anql:check_result(q4, 'tests/queries/ac-anql.pl').

:- end_tests(anql_AC).




% -------------------------
% Testcases -- Temporal RDF
% -------------------------


% test datasets
:- begin_tests(tRDF).
test(t1) :- ardf:run('tests/data/test.n4').
test(t1b) :- ardf:run('tests/data/test.rdf').
test(t2) :- ardf:run('tests/data/test_non_reified.rdf').
test(t3) :- ardf:run('tests/data/alainProst.rdf').
test(t4) :- ardf:run('tests/data/alainProst.n4').
test(t5) :- ardf:run('tests/data/employees.n4').
test(t6) :- ardf:run('tests/data/alainProst-pdStar.n4').
test(t7) :- ardf:run('tests/data/empty.n4').
test(t8) :- ardf:run('tests/data/alainProst-pdStar.n4', apDStar).
:- end_tests(tRDF).


:- begin_tests(anql_tRDF).
test(q1):-
        test_anql:check_result(q1, 'tests/queries/tRDF-anql.pl').
test(q2):-
        test_anql:check_result(q2, 'tests/queries/tRDF-anql.pl').
test(q3, [fail]):-
        test_anql:check_result(q3, 'tests/queries/tRDF-anql.pl').
test(q4):-
        test_anql:check_result(q4, 'tests/queries/tRDF-anql.pl').
test(q5):-
        test_anql:check_result(q5, 'tests/queries/tRDF-anql.pl').
test(q6):-
        test_anql:check_result(q6, 'tests/queries/tRDF-anql.pl').
test(q7):-
        test_anql:check_result(q7, 'tests/queries/tRDF-anql.pl').
test(q8):-
        test_anql:check_result(q8, 'tests/queries/tRDF-anql.pl').
test(q9):-
        test_anql:check_result(q9, 'tests/queries/tRDF-anql.pl').
test(q10):-
        test_anql:check_result(q10, 'tests/queries/tRDF-anql.pl').
test(q11, [fail]):-
        test_anql:check_result(q11, 'tests/queries/tRDF-anql.pl').
test(q12):-
        test_anql:check_result(q12, 'tests/queries/tRDF-anql.pl').
:- end_tests(anql_tRDF).




% ----------------------
% Testcases -- Fuzzy RDF
% ----------------------


:- begin_tests(fuzzyRDF).
test(f1) :- ardf:run('tests/data/fuzzy_test01.rdf').
% using the parser for quads we get extra quad..
test(f2) :- ardf:run('tests/data/fuzzy_test01.n4').
test(f3) :- ardf:run('tests/data/fuzzy_test01.rdf', apDStar).
test(f4) :- ardf:run('tests/data/fuzzy_test01.n4', apDStar).
:- end_tests(fuzzyRDF).



:- begin_tests(anql_fuzzyRDF).
test(q1):-
        test_anql:check_result(q1, 'tests/queries/fuzzyRDF-anql.pl').
test(q2):-
        test_anql:check_result(q2, 'tests/queries/fuzzyRDF-anql.pl').
test(q3):-
        test_anql:check_result(q3, 'tests/queries/fuzzyRDF-anql.pl').
test(q4):-
        test_anql:check_result(q4, 'tests/queries/fuzzyRDF-anql.pl').
test(q5):-
        test_anql:check_result(q5, 'tests/queries/fuzzyRDF-anql.pl').
test(q6):-
        test_anql:check_result(q6, 'tests/queries/fuzzyRDF-anql.pl').
test(q7):-
        test_anql:check_result(q7, 'tests/queries/fuzzyRDF-anql.pl').
test(q8):-
        test_anql:check_result(q8, 'tests/queries/fuzzyRDF-anql.pl').
test(q9):-
        test_anql:check_result(q9, 'tests/queries/fuzzyRDF-anql.pl').
test(q10):-
        test_anql:check_result(q10, 'tests/queries/fuzzyRDF-anql.pl').
test(q11):-
        test_anql:check_result(q11, 'tests/queries/fuzzyRDF-anql.pl').
test(q12, [fail]):-
        test_anql:check_result(q12, 'tests/queries/fuzzyRDF-anql.pl').
test(q13, [fail]):-
        test_anql:check_result(q13, 'tests/queries/fuzzyRDF-anql.pl').
:- end_tests(anql_fuzzyRDF).






% -----------------------
% Testcases -- Provenance
% -----------------------


:- begin_tests(provRDF).
test(p1) :- ardf:run('tests/data/prov_test01.n4').
test(p2) :- ardf:run('tests/data/prov_test01.n4', apDStar).
:- end_tests(provRDF).


% -------------------------------------
% Testcases -- compound time+provenance
% -------------------------------------


:- begin_tests(time_provRDF).
test(p1) :- ardf:run('tests/data/time_prov_test01.n4').
test(p2) :- ardf:run('tests/data/time_prov_test01.n4', apDStar).
test(p3) :- ardf:run('tests/data/time_prov_test02.n4', apDStar).
test(p4) :- ardf:run('tests/data/time_prov_test02.n4', apDStarInc).
:- end_tests(time_provRDF).
