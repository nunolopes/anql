% Rule1
rdf(S, P2, O2, A) <==
       rdf(S, P1, O1, A1),
       rdf(S, P2, O2, A2),
       supremum(A1, A2, A).

% Rule3
rdf(S2, P2, O2, A) <==
       rdf(S2, 'http://urq.deri.org/enterprise#isPartOf', S1, _),
       rdf(S1, P1, O1, A1),
       rdf(S2, P2, O2, A2),
       supremum(A1, A2, A).

% Rule4
rdf(S2, P2, O2, A) <==
       rdf(O2, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', S1, _),
       rdf(S1, P1, O1, A1),
       rdf(S2, P2, O2, A2),
       supremum(A1, A2, A).
