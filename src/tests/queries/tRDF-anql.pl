[
 query(q1,
       'tests/data/test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee ?t }",
       [row('http://example.org/foo#nuno', [[25, 40]])]),
 query(q2,
       'tests/data/test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee [[30,35]] }",
       [row('http://example.org/foo#nuno')]),
 query(q3,
       'tests/data/test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee [[20,35]] }",
        _),
 query(q4,
       'tests/data/test.n4',
       "PREFIX foo: <http://example.org/foo#>
      select * where { ?x a foo:Employee [[25,30][35,40]] }",
      [row('http://example.org/foo#nuno')]),
 query(q5,
       'tests/data/test.n4',
       "PREFIX foo: <http://example.org/foo#>
        select * where { ?x a foo:Employee ?t .
                         ?x a foo:Worker ?t . }",
       [row('http://example.org/foo#nuno', [[25, 40]])]),
 query(q6,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?c ?t }",
       [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]]),
        row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2010]])]),
 query(q7,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?company ?t
                         OPTIONAL { ?x :ownsCar ?car ?t }}",
        [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]], _G5211),
         row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]], 'http://example.org/peugeot'),
         row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2009]], 'http://example.org/fiat'),
         row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2010]], _G5228)]),
 query(q8,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { { ?x :worksFor ?company ?t }
                         union
                         { ?x :ownsCar ?car ?t } }",
        [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]], _G5211),
         row('http://example.org/axel', _G5175, [[2003, 2010]], 'http://example.org/peugeot'),
         row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2010]], _G5228),
         row('http://example.org/nuno', _G5192, [[2005, 2009]], 'http://example.org/fiat')]),
 query(q9,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?company ?t FILTER (?t = [[2006,2010]])}",
        [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]])]),
 query(q10,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?company ?t FILTER (beforeAny(?t,[[2011,2011]]))}",
        [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]]),
         row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2010]])]),
 query(q11,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?company ?t FILTER (beforeAny(?t,[[2008,2010]]))}",
        _),
 query(q12,
       'tests/data/employees.n4',
       "PREFIX : <http://example.org/>
        select * where { ?x :worksFor ?company ?t FILTER (beforeAll(?t,[[2011,2011]]))}",
        [row('http://example.org/axel', 'http://example.org/DERI', [[2006, 2010]]),
         row('http://example.org/nuno', 'http://example.org/DERI', [[2008, 2010]])])
].
