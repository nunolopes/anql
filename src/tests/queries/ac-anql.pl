[
 query(q1,
       'tests/data/ac_test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee ?t }",
       [row('http://example.org/foo#nuno', [[nl], [sk]])]),
 query(q2,
       'tests/data/ac_test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee [[nl]] }",
       [row('http://example.org/foo#nuno')]),
 query(q3,
       'tests/data/ac_test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee [[am]] }",
       _),
 query(q4,
       'tests/data/ac_test.n4',
       "select * where { ?s ?p ?o ?t }",
       [row('http://example.org/foo#nuno', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/foo#Employee', [[nl], [sk]]),
        row('http://example.org/bar#lives', 'http://www.w3.org/2000/01/rdf-schema#range', 'http://example.org/bar#city', [[nl, sk]])]
      ),
 query(q5,
       'tests/data/enterpriseData.n4',
       "PREFIX ent: <http://urq.deri.org/enterprise#>
        select * where { ?x a ent:Organisation :[[mobrien]] }",
       _)
].
