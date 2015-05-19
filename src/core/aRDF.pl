% Prototype for temporal RDF saturation

:- module(ardf, [read_kb/2, clean_kb/1, saturate/2, run/1, run/2, run/3, run_stats/2, run_stats/3]).

:- use_module(rdf_turtle).
:- use_module(sparql).

:- op(1190,xfx,user:(<==)).
:- op(1200,xfx,user:(@)).

% --------------
% Term expansion
% --------------

% rdf_ns('http://www.w3.org/1999/02/22-rdf-syntax-ns#').
% rdfs_ns('http://www.w3.org/2000/01/rdf-schema#').
ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
ns(owl, 'http://www.w3.org/2002/07/owl#').

expand_rdf_structures(Term, ExpandedTerm) :-
        var(Term), !, ExpandedTerm = Term.
expand_rdf_structures(Term, ExpandedTerm) :-
        atomic(Term), !, ExpandedTerm = Term.
expand_rdf_structures(Term, ExpandedTerm) :-
        Term = NS:RTerm, atomic(RTerm), !,
        ns(NS, NSR),
        atom_concat(NSR, RTerm, ExpandedTerm).
% expand_rdf_structures(Term, ExpandedTerm) :-
%       Term = rdfs:RTerm, atomic(RTerm), !,
%       rdfs_ns(NS),
%       atom_concat(NS, RTerm, ExpandedTerm).
expand_rdf_structures(Term, ExpandedTerm) :-
        compound(Term),
        Term =.. [Name|Args],
        expanded_args(Args, Args1),
        ExpandedTerm =.. [Name|Args1].

expanded_args([], []).
expanded_args([A|As], [E|Es]) :-
        expand_rdf_structures(A, E),
        expanded_args(As, Es).


% replace every rdf:_, rdfs:_ term at any depth in every term
ardf:term_expansion(Term, ExpandedTerm) :-
        expand_rdf_structures(Term, ExpandedTerm).


% ----------
% saturation
% ----------

% saturate(+Modulname, +Rules)
% Saturates modul Modulname according to the predefined rules in Rules (default: aRDF_rules.rl).
% The saturated triples are placed in modul Modulname.
saturate(Modulname, RulesFile) :-
        load_rules_file(RulesFile),
        saturate_kb(Modulname).


% saturate_kb(+Modulname)
saturate_kb(Modulname) :-
        bb_put(inferred_count, 0),
        bb_put(current_module, Modulname),
        deduce(Modulname),
        bb_get(inferred_count, C),
        log('Reasoning finished, ~w inference steps.~n',[C]).

% deduce(+Modulname)
deduce(Modulname) :-
        deduce_1(Modulname),
        (
         % check fixpoint
         bb_get(was_inferred,1) ->
         bb_put(was_inferred,0),
         deduce(Modulname)
        ;
         % reasoning has been finished
         true
        ).

% deduce_1(+Modulname)
deduce_1(Modulname) :-
        rl: (_C @ P <== B), % pick a rule 
        call_predicate(B), % execute body
        safe_assert(P, Modulname), % assert head if body succeeds
        fail.
deduce_1(_).

% Only for easier debugging.
call_predicate(G) :-
        G.

% quad(+Module, ?S, ?P, ?O, ?V)
% Matches an RDF quad in the current knowledge base.
quad_match(Module, S, P, O, V) :-
        Module:quad(S, P, O, V).

quad_match0(S, P, O, V) :-
        bb_get(current_module, Module),
        quad_match(Module, S, P, O, V).

% ----------
% loading kb
% ----------

% run(+Filename)        
run(Filename):-
        run(Filename, ardfs).

run(Filename, Entailment):-
        entailment(Entailment, E),
        run(Filename, E, triples).


% run_stats(+Filename, -Stats)        
run_stats(Filename, Stats) :-
        run_stats(Filename, ardfs, Stats).

run_stats(Filename, Entailment, Stats):-
        entailment(Entailment, E),
        run_stats(Filename, E, triples, Stats).


% run(+Filename, +ruleset)        
entailment(ardfs, 'rules/aRDFS_rules.rl').
entailment(apDStar, 'rules/pdStar_rules.rl').
entailment(apDStarInc, 'rules/pdStar_rules_inc_trac.rl').
entailment(enterprise, 'rules/enterpriseRules.rl').


% specifically rerun example and rules
rerun :- 
        bb_get(last_file, Filename),
        bb_get(last_rules, Rules),
        Rules \= in_progress,   % there was an error running the program, always rerun!
        store_dataset(Filename, in_progress),
        run(Filename, Rules, triples).


% run(+Filename, +ruleFile, +module)        
run(Filename, Rules, Mod):-
        check_dataset(Filename, Rules), !,
        clean_kb(Mod),
        read_kb(Filename, Mod),
        saturate(Mod, Rules),
        store_dataset(Filename, Rules).
run(_,_,_).


% run_stats(+Filename, +ruleFile, +module, -stats)        
run_stats(Filename, Rules, Mod, [load_time=LoadT, load_triples=LT, infer_time=InferT, infer_triples=IT]):-
%        check_dataset(Filename, Rules), !,
        clean_kb(Mod),
        get_time(ReadS),
        read_kb(Filename, Mod),
        get_time(ReadE),
        LoadT is ReadE - ReadS, 
        count_kb(LT),
        saturate(Mod, Rules),
        get_time(InferE),
        count_kb(IT),
        InferT is InferE - ReadE, 
        store_dataset(Filename, Rules).
%run_stats(_,_,_, []).


% save the last file loaded and infered ruleset. when querying don't load more than once.
check_dataset(Filename, Rules) :- bb_get(last_file, Filename), bb_get(last_rules, Rules), !, fail.
check_dataset(Filename, _Rules):- store_dataset(Filename, in_progress).


store_dataset(Filename, Rules) :-
        bb_put(last_file, Filename), bb_put(last_rules, Rules).


write_kb :-
        write_kb(triples).

write_kb(Module) :-
        Module:quad(S,P,O,A),
        writeq(rdf(S, P, O, A)),nl,
        fail.
write_kb(_).


%  count the triples in the KB
count_kb(C) :-
        count_kb(triples, C).

count_kb(Module, C) :-
        findall(S,Module:quad(S,_,_,_), L),
        length(L, C).


% read_kb(+Filename, +Modulname)
% Reads the turtle KB located in Filename and asserts the triples in module Modulname.
read_kb(Filename, Modulname) :-
        rdf_load_turtle(Filename, Triples, [domain(D)]),
        load_domain(D),
        triples2quads(Modulname, Triples).

% load_domain(-Dom)
% loads a domain specified in the input RDF file
load_domain(Dom) :-
        nonvar(Dom), !, 
        map_domain(Dom, Module),
        use_module(Module),
        bb_put(domain, Module).
% can read domains from other places.
load_domain(_) :-
        log('No domain annotation. Please use @domain in the input RDF file.~n', []),
        halt(1).

% map_domain(Dom, Module)
% maps a URI to a module
map_domain('http://anql.deri.org/domains#fuzzy', 'fuzzy').
map_domain('http://anql.deri.org/domains#time', 'temporal').
map_domain('http://anql.deri.org/domains#provenance', 'provenance').
map_domain('http://anql.deri.org/domains#time%20provenance', 'time_prov').
map_domain('http://anql.deri.org/domains#ac', 'ac').


% assert_triples(+Triples, +Modulname)
% Assert the elements of the list Triples into modul Modulname.
assert_triples([], _).
assert_triples([T|Ts], Modulname) :-
        asserta(Modulname:T),
        assert_triples(Ts, Modulname).


% clean_kb(+Modulname)
% Cleans the triples from KB identified by modulname Modulename.
clean_kb(Modulname) :-
        retractall(Modulname:quad(_,_,_,_)).


% triples2quads(+Module, +Triples).
% converts the triples in module 'ModName' into quads, using the default value
% for non anotated triples and asserts in Module
triples2quads(Module, Triples):-
        retractall(tmp:rdf(_,_,_)),
        retractall(tmp:rdf(_,_,_,_)),
        assert_triples(Triples, tmp), 
        triples2quads(Module).

% triples2quads(+Mod)
% 
triples2quads(Module):-
        reified(tmp, S, P, O, V),
        assert_quad(Module, S, P, O, V),
        fail.
triples2quads(Module):-
        default(D),
        % get non-annotated triples
        tmp:rdf(S, P, O),
        % assert corresponding quad with default value
        assert_quad(Module, S, P, O, D),
        fail.
triples2quads(Module):-
        % get quads 
        tmp:rdf(S, P, O, AL),
        (var(AL) -> default(A); extract_label(AL, A)),
        % assert corresponding quad with default value
        assert_quad(Module, S, P, O, A),
        fail.
triples2quads(_).


% reified(+Module, ?S, ?P, ?O, ?V)
% Matches an anotated reified RDF triple in the current knowledge base.
% retracts the all the found reified triple (possibly non-annotated) 
reified(Module, S, P, O, V) :-
        once(ardf_label(L)),

        % find a reified triple
        Module:rdf(X, rdf:type, rdf:'Statement', _),
        (
         Module:rdf(X, rdf:subject, S, _),
         Module:rdf(X, rdf:predicate, P, _),
         Module:rdf(X, rdf:object, O, _) -> true
        ),
        % remove it
        (
         retract(Module:rdf(X, rdf:type, rdf:'Statement', _)), 
         retract(Module:rdf(X, rdf:subject, S, _)), 
         retract(Module:rdf(X, rdf:predicate, P, _)),
         retract(Module:rdf(X, rdf:object, O, _)) -> true
        ),

        % check if it is annotated, fail if not
        once(Module:rdf(X, L, V0, _)),

        % remove annotation
        once(retract(Module:rdf(X, L, V0, _))),

        % extract label
        extract_label(V0, V).
                

% --------------
% loading rules
% --------------

% load_rules
load_rules_file(RulesFile) :-
        open(RulesFile, read, Handle),
        retractall(rl:(_@_)),
        bb_put(rule_count, 0),
        load_rules(Handle),
        close(Handle).

% load_rules(+Handle)
load_rules(Handle) :-
        read(Handle, Term),
        (
         Term \= end_of_file ->
         process_rule_term(Term),
         load_rules(Handle)
        ;
         true
        ).

% process_rule_term(+Term)
process_rule_term(Term) :-
        (
         Term = (H <== B)->
         transformed_head(H, HT),
         transformed_term(B, BT), 
         bb_get(rule_count, RC),
         RC1 is RC+1,
         assert(rl:(RC1 @ HT <== BT)),
         bb_put(rule_count, RC1)
        ;
         log("Warning: Cannot process rule: ~w~n",[Term]),
         fail
        ).

transformed_term((T1, T2), (TT1, TT2)) :-
        !,
        transformed_term(T1, TT1),
        transformed_term(T2, TT2).
transformed_term(Term, TTerm) :-
        transformed_term0(Term, TTerm).

transformed_term0(rdf(A, B, C, D), ardf:quad_match0(A0, B0, C0, D)) :-
        !,
        transformed_argument(A, A0),
        transformed_argument(B, B0),
        transformed_argument(C, C0).
transformed_term0(T, ardf:T).

transformed_head(rdf(A, B, C, D), quad(A0, B0, C0, D)) :-
        transformed_argument(A, A0),
        transformed_argument(B, B0),
        transformed_argument(C, C0).

transformed_argument(Term, Res) :-
        (
         var(Term) -> Res = Term
        ;
         Term = NS:RTerm ->
         ns(NS, NSR),
         atom_concat(NSR, RTerm, Res)
        ;
        %  Term = rdf:RTerm ->
        %  rdf_ns(NS),
        %  atom_concat(NS, RTerm, Res)
        % ;
        %  Term = rdfs:RTerm ->
        %  rdfs_ns(NS),
        %  atom_concat(NS, RTerm, Res)
        % ;
         Res = Term
        ).

% --------------------
% domain predicates
% --------------------

ardf_label(L) :-
        bb_get(domain, Dom),
        Dom:ardf_label(L).


extract_label(Label, Term) :-
        bb_get(domain, Dom),
        Dom:extract_label(Label, Term).

different_labels(L1, L2):-        
        bb_get(domain, Dom),
        Dom:different_labels(L1, L2).

infimum(TL1, TL2, TLr) :-
        bb_get(domain, Dom),
        Dom:infimum(TL1, TL2, TLr).

supremum(V1, V2, V) :-
        bb_get(domain, Dom),
        Dom:supremum(V1, V2, V).

default(D) :-
        bb_get(domain, Dom),
        Dom:default(D).

lower_annotation(QueryAnnotation, DataAnnotation, ResultAnnotation) :-
        bb_get(domain, Dom),
        Dom:lower_annotation(QueryAnnotation, DataAnnotation, ResultAnnotation).


% --------------------
% auxiliary predicates
% --------------------
% log(+Message, +Arguments)
log(Message, Arguments) :-
        format(Message, Arguments).


% bb_put(+Key, +Value)
% bb_get(+Key, -Value)
bb_put(X,Y) :- nb_setval(X,Y).
bb_get(X,Y) :- catch(nb_getval(X,Y), _E, fail).

% safe_assert(+Term, +Modulname)
safe_assert(Term, Modulname) :-
        Term = quad(S,P,O,V),
        (
         % term has been found, possibly with a different temporal value
         quad_match(Modulname, O, owl:sameAs, S, V1) ->
         update_quad(Term, V1, Modulname) 
        ;
         quad_match(Modulname, S, P, O, V1) ->
         update_quad(Term, V1, Modulname) 
        ;
         bb_put(was_inferred, 1),
         assert_quad(Modulname, S, P, O, V),
%        writeq(assert_quad(Modulname, S, P, O, V)),nl,
         update_infered_count
         ).

update_infered_count:-
        bb_get(inferred_count, C),
        C1 is C+1,
        bb_put(inferred_count, C1).


update_quad(quad(S, P, O, V), VV, Module):-
        supremum(V, VV, VR),
        (
         different_labels(VR, VV) ->
%         writeq(ardf:supremum(V, VV, VR)),nl,
         update_quad0(quad(S,P,O,VR), Module)
        ;
         true
        ).


update_quad0(quad(S, P, O, V), Module):-
        % remove old value for the label
        retractall(Module:quad(S,P,O, _)),
        assert_quad(Module, S, P, O, V),
 %       writeq(update_quad0(quad(S, P, O, V), Module)),nl,
        bb_put(was_inferred, 1),
        update_infered_count.


assert_quad(Module, S, P, O, V) :-
        asserta(Module:quad(S, P, O, V)).

infimum(TL1, TL2, TL3, TLr):-
        infimum(TL1, TL2, TLt),
        infimum(TLt, TL3, TLr). 

infimum(TL1, TL2, TL3, TL4, TLr):-
        infimum(TL1, TL2, TLt1),
        infimum(TLt1, TL3, TLt2),
        infimum(TLt2, TL4, TLr). 


% --------------------
% export predicates
% --------------------
export_quads(Module, Filename):-
        bb_put(bnode_id, 1),
        findall(quad(S, P, O, V), Module:quad(S, P, O, V), Quads), 
        quads2triples(Quads, [], Triples),
        open(Filename, write, Stream),
        rdf_write_xml(Stream, Triples),
        close(Stream).


quads2triples([], Triples, Triples).
quads2triples([quad(S,P,O,V)|Quads], It, Triples) :-
        default(D),
        D == V, !,
        quads2triples(Quads, [rdf(S,P,O)|It], Triples).
quads2triples([quad(S, P, O, V)|Quads], It, Triples) :-
        generate_node_id(X),
        ardf_label(L),
        write_to_chars([V], Cs),
        atom_codes(VA, Cs),
        quads2triples(Quads,
                      [rdf(X,rdf:type,rdf:'Statement'),
                       rdf(X,rdf:subject,S),
                       rdf(X,rdf:predicate,P),
                       rdf(X,rdf:object,O),
                       rdf(X,L,literal(VA))|It],
                      Triples).


generate_node_id(X) :-
        bb_get(bnode_id, C),
        C1 is C + 1, 
        bb_put(bnode_id, C1),
        number_codes(C, Cs),
        atom_codes(A, Cs),
        atom_concat('__local', A, X).



