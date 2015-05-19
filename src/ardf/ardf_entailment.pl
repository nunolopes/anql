
:- module(ardf_entailment,
	  [ rdf/5 ]).
% :- use_module(rdfql_runtime).			% runtime tests
:- use_module(library('semweb/rdf_db'),
	      [ rdf_global_id/2,
		rdf_subject/1
	      ]).


% term_expansion((rdf(S0, P0, O0, A0) :- Body0),
% 	       (rdf(S,  P,  O,  A)  :- triples:Body)) :-
% 	rdf_global_id(S0, S),
% 	rdf_global_id(P0, P),
% 	rdf_global_id(O0, O),
% 	rdf_global_id(A0, A),
% 	expand_goal(Body0, Body).

% rdf(S, P, O, A, NA) :-
%         triples:quad(S, P, O, A1),
%         infimum_var(A, A1, NA).    % we want the query annotation to be smaller than the value of the triple
rdf(S, P, O, AQ, NA) :-
        triples:quad(S, P, O, AD),
        lower_annotation(AQ, AD, NA).
% % add result "P a property" ?
% rdf(S, rdf:type, rdf:'Property', D, _) :-
%         quad(_, S, _, _),
%         ardf:default(D),
% 	\+ quad(S, rdf:type, rdf:'Property', _).
rdf(S, rdf:type, rdfs:'Resource', D, _) :-
	rdf_subject(S),
        ardf:default(D),
	\+ quad(S, rdf:type, rdfs:'Resource', _).



% infimum_var(AQuery, ARDF, A) :- var(AQuery), !, A = ARDF.
% infimum_var(AQuery, ARDF, A) :- ardf:infimum(AQuery, ARDF, A).
% % infimum_var(AQuery, ARDF, A) :- ardf:infimum(AQuery, ARDF, A), A = AQuery.
% % infimum_var(AQuery, ARDF, A) :- ardf:supremum(AQuery, ARDF, A), A = ARDF.


lower_annotation(AQuery, ARDF, ARes) :- var(AQuery), !, ARes = ARDF.
lower_annotation(AQuery, ARDF, ARes) :- ardf:lower_annotation(AQuery, ARDF, ARes).


sparql_true(A):- sparql_runtime:sparql_true(A).

		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	serql:entailment/2.

serql:entailment(ardf_entailment, ardf_entailment).
