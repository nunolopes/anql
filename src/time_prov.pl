
:- module(time_prov, []).

:- use_module('core/compound').
:- initialization(compound:init('temporal', 'provenance')).



:- begin_tests(time_prov_reduce).
test(t1) :- compound:reduce([ ([[1981,1983]], [['wiki.com']]) , ([[1981,1983]], [['wrong.com']]) ],
                            [ ([[1981,1983]], [['wiki.com'],['wrong.com']]) ]).

test(t2) :- compound:reduce([ ([[1981,1983]], [['wiki.com']]) , ([[1981,1985]], [['wrong.com']]) ],
                            [ ([[1981,1983]], [['wiki.com']]) , ([[1981,1985]], [['wrong.com']]) ]).

test(t3) :- compound:reduce([ ([[1981,1983]], [['wiki.com']]) , ([[1981,1985]], [['wrong.com']]) ],
                            [ ([[1981,1983]], [['wiki.com']]) , ([[1981,1985]], [['wrong.com']]) ]).

test(t4) :- compound:reduce([ ([[1981,1983]], [['wiki.com']]) , ([[inf,sup]], [['wrong.com']])], _C).
:- end_tests(time_prov_reduce).


:- begin_tests(time_prov_saturate).
test(t1) :- compound:normalise([ ([[1998,2006]], [['wiki.com']]) , ([[2001,2011]], [['wrong.com']]) ],
                               [ ([[1998, 2006]], [['wiki.com']]), ([[1998, 2011]], [['wiki.com', 'wrong.com']]), ([[2001, 2006]], [['wiki.com'], ['wrong.com']]), ([[2001, 2011]], [['wrong.com']])]).
test(t2) :- compound:normalise([ ([[inf,sup]], [['wiki.com']]) , ([[2001,2011]], [['wrong.com']]) ],
                               [ ([[2001, 2011]], [['wiki.com'], ['wrong.com']]), ([[inf, sup]], [['wiki.com']])]).
test(t3) :- compound:normalise([ [[inf,sup]] ',' [[true]],[[inf,sup]] ',' [['evil.com','wiki.com','workont.org']]], [ ([[inf, sup]], [[true]])]).
:- end_tests(time_prov_saturate).
