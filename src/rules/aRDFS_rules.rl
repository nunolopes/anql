% Rules for annotated RDF saturation

% Subproperty 
rdf(P1, rdfs:subPropertyOf, P3, V) <==
       rdf(P1, rdfs:subPropertyOf, P2, V1),
       rdf(P2, rdfs:subPropertyOf, P3, V2),
       infimum(V1, V2, V). 

rdf(S, P2, O, V) <==
       rdf(S, P1, O, V1),
       rdf(P1, rdfs:subPropertyOf, P2, V2),
       infimum(V1, V2, V). 

% Subclass 
rdf(O, rdf:type, C2, V) <==
       rdf(O, rdf:type, C1, V1),
       rdf(C1, rdfs:subClassOf, C2, V2),
       infimum(V1, V2, V). 

rdf(C1, rdfs:subClassOf, C3, V) <==
       rdf(C1, rdfs:subClassOf, C2, V1),
       rdf(C2, rdfs:subClassOf, C3, V2),
       infimum(V1, V2, V). 


% Typing
rdf(X, rdf:type, C, V) <==
       rdf(P, rdfs:domain, C, V1),
       rdf(X, P, Y, V2),
       infimum(V1, V2, V). 

rdf(Y, rdf:type, C, V) <==
       rdf(P, rdfs:range, C, V1),
       rdf(X, P, Y, V2),
       infimum(V1, V2, V). 

% Implicit typing
rdf(X, rdf:type, C, V) <==
       rdf(P1, rdfs:domain, C, V1),
       rdf(P2, rdfs:subPropertyOf, P1, V2),
       rdf(X, P2, Y, V3),
       infimum(V1, V2, Vi),
       infimum(Vi, V3, V). 

rdf(Y, rdf:type, C, V) <==
       rdf(P1, rdfs:range, C, V1),
       rdf(P2, rdfs:subPropertyOf, P1, V2),
       rdf(X, P2, Y, V3),
       infimum(V1, V2, Vi),
       infimum(Vi, V3, V). 


% Generalisation
rdf(S, P, O, V) <==
       rdf(S, P, O, V1),
       rdf(S, P, O, V2),
       supremum(V1, V2, V). 
