[
query(q1,
      'tests/data/fuzzy_test01.n4',
     'PREFIX bar: <http://example.org/ns#>
      select * where { ?s a bar:city }',
       [row('http://example.org/ns#cork'),row('http://example.org/ns#galway')]),
 query(q2,
       'tests/data/fuzzy_test01.n4',
      'PREFIX bar: <http://example.org/ns#>
      select * where { ?s a bar:city 0.5 }',
       [row('http://example.org/ns#cork'), row('http://example.org/ns#galway')]),
 query(q3,
       'tests/data/fuzzy_test01.n4',
       'PREFIX bar: <http://example.org/ns#>
      select * where { ?s a bar:city 0.8 }',
       [row('http://example.org/ns#galway')]),
 query(q4,
       'tests/data/fuzzy_test01.n4',
      'PREFIX bar: <http://example.org/ns#>
      select * where { ?s a bar:city ?b }',
       [row('http://example.org/ns#cork', 0.7),row('http://example.org/ns#galway', 0.8)]),
  query(q5,
       'tests/data/fuzzy_test01.n4',
      'PREFIX bar: <http://example.org/ns#>
       select * where { bar:cork a bar:city ?t . bar:galway a bar:city ?t }',
       [row(0.7)]),
  query(q6,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
       select * where { bar:galway a bar:city ?t . bar:cork a bar:city ?t }',
       [row(0.7)]),
  query(q7,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { { bar:galway a bar:city ?t } UNION { bar:cork a bar:city ?t } }',
       [row(0.7),row(0.8)]),
  query(q8,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { { bar:galway a bar:city ?t1 } UNION { bar:cork a bar:city ?t } }',
       [row(0.7, _),row(_,0.8)]),
  query(q9,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { { bar:galway a bar:city ?t } OPTIONAL { bar:cork a bar:city ?t } }',
       [row(0.7),row(0.8)]),
  query(q10,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { { bar:galway a bar:city ?t. bar:cork a bar:city ?t } OPTIONAL { bar:cork a bar:city1 ?t } }',
       [row(0.7)]),
  query(q11,
       'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { { ?city a bar:city ?t } OPTIONAL { ?city1 a bar:city ?t } }',
       [row('http://example.org/ns#cork', 0.7, _G78), row('http://example.org/ns#cork', 0.7, 'http://example.org/ns#cork'), row('http://example.org/ns#cork', 0.7, 'http://example.org/ns#galway'), row('http://example.org/ns#galway', 0.7, 'http://example.org/ns#cork'), row('http://example.org/ns#galway', 0.8, _G48), row('http://example.org/ns#galway', 0.8, 'http://example.org/ns#galway')]),
  query(q12,
        'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { ?city a bar:city; bar:population ?pop ?t FILTER(?pop > 200000)}',
       []),
    query(q13,
        'tests/data/fuzzy_test01.n4',
        'PREFIX bar: <http://example.org/ns#>
         select * where { ?city a bar:city; bar:population ?pop ?t FILTER(?t > 0.9)}',
       [])


].
