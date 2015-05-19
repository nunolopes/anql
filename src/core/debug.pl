:- initialization(main).


main:-
        guitracer,
%        trace,
        consult(aRDF),
                                %        spy(triples2quads/1),
                                %        spy(infimum/3),

%        spy(ardf:safe_assert/2),
        spy(ardf:deduce_1/1),
        spy(ardf:update_quad/3),
%        ardf:run('tests/prov_test01.n4'),
        ardf:run('tests/time_prov_test02.n4', apDStarInc),
        listing(triples:_),
        
        % use_module(provenance),
        % spy(provenance:infimum/3),
        % provenance:infimum( [['onto.org', 'alain.org']],  [['alain.org'], ['onto.org']], X), %[['onto.org','alain.org'],['onto.org']]
        
        % use_module(provenance),
        % spy(provenance:supremum/3),
        % provenance:supremum( [['alain.org', 'foaf.com']], [['alain.org', 'onto.org']], X),
        % writeq(X),nl,

        halt.


