% Rules for annotated pD* saturation



%%  ================================================================================
%%  Annotated D* entailment rules 
%%  ================================================================================


%% % a-gen    
%% rdf(S, P, O, A) <==
%%     rdf(S, P, O, A1),
%%     rdf(S, P, O, A2),
%%     infimum(A1, A2, A). 
     
%% % a-lg , Bl is singleton. 
%% rdf(V, P, Bl, A) <==
%%    rdf(V, P, L, A). 
%% %  $l\in\AL$   

%% % a-gl      Bl is singleton. 
%% rdf(V, P, L, A) <==
%%     rdf(V, P, Bl, A).
%% % $l\in\AL$
 
% a-rdf1    
rdf(P, rdf:type, rdf:'Property', D) <==
    ardf:default(D),
    rdf(_, P, _, _),
    P \= idClash,
    P \= disjClash,
    P \= membClash.
      
% a-rdf1x  
rdf(P, rdf:type, rdf:'Property', D) <==
   ardf:default(D),
   rdf(P, rdf:type, rdf:'Property', D1),
   D \== D1.
      
%% % a-rdf2-D !!!!! 
%% rdf(Bl, rdf:type, A, D) <==
%%     ardf:default(D),
%%     rdf(_V, _P, L, _).
%% % $l=(s, a)\in\L_D^+$
 
%% % a-rdfs1  
%% rdf(Bl, rdf:type, rdf:'Literal', D) <==
%%     ardf:default(D),
%%     rdf(_V, _P, L, _).
%% % $l$ plain literal
 
%% % a-rdfs1x 
%% rdf(V, rdf:type, rdf:'Literal', D) <==
%%     ardf:default(D),
%%    rdf(V, rdf:type, rdf:'Literal', _).
      
% a-rdfs2  
rdf(V, rdf:type, U, A) <==
    rdf(P, rdf:domain, U, A1),
    rdf(V, P, _W, A2),
    infimum(A1, A2, A).
      
% a-rdfs3  
rdf(W, rdf:type, U, A) <==
    rdf(P, rdf:range, u, A1),
    rdf(_V, P, W, A2),
    infimum(A1, A2, A).
% $w\in\AUB$
 
% a-rdfs4a 
rdf(V, rdf:type, rdfs:'Resource', A) <==
    rdf(V, _P, _W, A).
      
% a-rdfs4b 
rdf(W, rdf:type, rdfs:'Resource', A) <==
 rdf(_V, _P, W, A).
% $w\in\AUB$
 
% a-rdfs5  
rdf(V, rdfs:subPropertyOf, U, A) <==
    rdf(V, rdfs:subPropertyOf, W, A1),
    rdf(W, rdfs:subPropertyOf, U, A2),
    infimum(A1, A2, A).

% trouble?
% a-rdfs6  
rdf(V, rdfs:subPropertyOf, V, D) <==
    ardf:default(D),
    rdf(V, rdf:type, rdf:'Property', _).
      
% a-rdfs7x 
rdf(V, Q, W, A1) <==
    rdf(P, rdfs:subPropertyOf, Q, A1),
    P \== Q,
    rdf(V, P, W, A2).
% $q\in\AUB$
 
% a-rdfs8  
rdf(V, rdfs:subClassOf, rdfs:'Resource', D) <==
    ardf:default(D),
    rdf(V, rdf:type, rdfs:'Class', _A).
      
% a-rdfs8x 
rdf(V, rdf:type, rdfs:'Class', D) <== %% MAY BE USELESS
    ardf:default(D),
   rdf(V, rdf:type, rdfs:'Class', _A).
      
% a-rdfs9  
rdf(U, rdf:type, W, A) <==
    rdf(V, rdfs:subClassOf, W, A1),
    V \== W,
    rdf(U, rdf:type, V, A2),
    infimum(A1, A2, A).
      
% trouble?
% a-rdfs10 
rdf(V, rdfs:subClassOf, V, D) <==
    ardf:default(D),
    rdf(V, rdf:type, rdfs:'Class', _A).
      
% a-rdfs11 
rdf(V, rdfs:subClassOf, U, A) <==
    rdf(V, rdfs:subClassOf, W, A1),
    rdf(W, rdfs:subClassOf, U, A2),
    infimum(A1, A2, A).
      
% a-rdfs12 
rdf(V, rdfs:subPropertyOf, rdfs:'Member', A) <==
    rdf(V, rdf:type, rdfs:'ContainerMembershipProperty', A).
  
% a-rdfs13 
rdf(V, rdfs:subClassOf, rdf:'Literal', D) <==
    ardf:default(D),
    rdf(V, rdf:type, rdfs:'Datatype', _).
      
% a-rdfs13x
rdf(V, rdf:type, rdfs:'Datatype', D) <==
    ardf:default(D),
    rdf(V, rdf:type, rdfs:'Datatype', _).
    



%%  ================================================================================
%%  Annotated P entailment rules 
%%  ================================================================================

% a-RDFP1: sameAs 
rdf(V, owl:sameAs, W, A) <==
       rdf(P, rdf:type, owl:'FunctionalProperty', A1),
       rdf(U, P, V, A2),
       rdf(U, P, W, A3),
       V \= W,            %  used not to assert (u sameAs w) and (w sameAs u)
       infimum(A1, A2, A3, A). 
% V \in U \cup B


% a-RDFP2: inverseFunctional
rdf(U, owl:sameAs, V, A) <==
       rdf(P, rdf:type, owl:'InverseFunctionalProperty', A1),
       rdf(U, P, W, A2),
       rdf(V, P, W, A3),
       U \= V,                %  used not to assert (u sameAs w) and (w sameAs u)
       infimum(A1, A2, A3, A). 


% a-RDFP3: SymmetricProperty
rdf(W, P, V, A) <==
       rdf(P, rdf:type, owl:'SymmetricProperty', A1),
       rdf(V, P, W, A2),
       infimum(A1, A2, A). 
% W \in U \cup B


% a-RDFP4: TransitiveProperty
rdf(U, P, W, A) <==
       rdf(P, rdf:type, owl:'TransitiveProperty', A1),
       rdf(U, P, V, A2),
       rdf(V, P, W, A3),
       infimum(A1, A2, A3, A). 


% a-RDFP5a:
% gives a lot of inference steps
rdf(V, owl:sameAs, V, D) <==
       ardf:default(D),
       rdf(V, _P, _, _). 


% a-RDFP5b: 
rdf(W, owl:sameAs, W, D) <==
       ardf:default(D),
       rdf(_, _P, W, _). 
% W \in U \cup B


% a-RDFP6: 
rdf(W, owl:sameAs, V, A) <==
       rdf(V, owl:sameAs, W, A). 
% W \in U \cup B


% a-RDFP7: 
rdf(U, owl:sameAs, W, A) <==
       rdf(U, owl:sameAs, V, A1),
       rdf(V, owl:sameAs, W, A2),
       infimum(A1, A2, A). 
% W \in U \cup B


% a-RDFP8ax: 
rdf(W, Q, V, A) <==
       rdf(P, owl:inverseOf, Q, A1),
       rdf(V, P, W, A2),
       infimum(A1, A2, A). 
% W, Q \in U \cup B


% a-RDFP8bx: 
rdf(W, P, V, A) <==
       rdf(P, owl:inverseOf, Q, A1),
       rdf(V, Q, W, A2),
       infimum(A1, A2, A). 
% W \in U \cup B


% a-RDFP9: 
rdf(V, rdfs:subClassOf, W, A) <==
       rdf(V, rdf:type, rdfs:'Class', _),
       rdf(V, owl:sameAs, W, A). 


% a-RDFP10: 
rdf(V, rdfs:subPropertyOf, W, A) <==
       rdf(V, rdf:type, rdf:'Property', _),
       rdf(V, owl:sameAs, W, A). 


% a-RDFP11: 
rdf(U1, P, V1, A) <==
       rdf(U, P, V, A1),
       rdf(U, owl:sameAs, U1, A2),
       rdf(V, owl:sameAs, V1, A3),
       infimum(A1, A2, A3, A). 
% U1 \in U \cup B


% a-RDFP12a: 
rdf(V, rdfs:subClassOf, W, A) <==
       rdf(V, owl:equivalentClass, W, A).

% a-RDFP12b: 
rdf(W, rdfs:subClassOf, W, A) <==
       rdf(V, owl:equivalentClass, W, A).

% a-RDFP12c: 
rdf(V, owl:equivalentClass, W, A) <==
       rdf(V, rdfs:subClassOf, W, A1),
       rdf(W, rdfs:subClassOf, V, A2),
       infimum(A1, A2, A). 


% a-RDFP13a: 
rdf(W, rdfs:subPropertyOf, V, A) <==
       rdf(V, owl:equivalentProperty, W, A).

% a-RDFP13b: 
rdf(V, rdfs:subPropertyOf, W, A) <==
       rdf(V, owl:equivalentProperty, W, A).

% a-RDFP13c: 
rdf(V, owl:equivalentProperty, W, A) <==
       rdf(V, rdfs:subPropertyOf, W, A1),
       rdf(W, rdfs:subPropertyOf, V, A2),
       infimum(A1, A2, A). 


% a-RDFP14a: 
rdf(U, rdf:type, V, A) <==
       rdf(V, owl:hasValue, W, A1),
       rdf(V, owl:onProperty, P, A2),
       rdf(U, P, W, A3),
       infimum(A1, A2, A3, A). 


% a-RDFP14b: 
rdf(U, P, W, A) <==
       rdf(V, owl:hasValue, W, A1),
       rdf(V, owl:onProperty, P, A2),
       rdf(U, rdf:type, V, A3),
       infimum(A1, A2, A3, A). 
% P \in U \cup B


% a-RDFP15: 
rdf(U, rdf:type, V, A) <==
       rdf(V, owl:someValuesFrom, W, A1),
       rdf(V, owl:onProperty, P, A2),
       rdf(U, P, X, A3),
       rdf(X, rdf:type, K, A4),
       infimum(A1, A2, A3, A4, A). 



% a-RDFP16: 
rdf(X, rdf:type, W, A) <==
       rdf(V, owl:allValuesFrom, W, A1),
       rdf(V, owl:onProperty, P, A2),
       rdf(U, rdf:type, V, A3),
       rdf(U, P, X, A4),
       infimum(A1, A2, A3, A4, A). 
% X \in U \cup B


%%  ================================================================================
%%  Annotated inconsistency tracking
%%  ================================================================================

% % a-dclash  
% rdf(b_l,\rtype,rsLiteral}{\lambda} & $b_l\in \AB$ assigned to an ill-formed literal $l$ &
% \fuzzyg{b_l,\dClash,l}{\lambda} 

% a-pclash1 
rdf(X,idClash,Y, A) <==
        rdf(X, owl:sameAs, Y, A1), 
        rdf(X, owl:differentFrom, Y, A2), 
        infimum(A1, A2, A). 

% a-pclash2 
rdf(C, disjClash, D, A) <==
        rdf(X, rdf:type, C, A1), 
        rdf(X, rdf:type, D, A2), 
        rdf(C, owl:disjointWith, D, A3), 
        infimum(A1, A2, A3, A). 

% a-pclash3 
rdf(X, membClash, C, A) <==
        rdf(X, rdf:type, C, A1), 
        rdf(X, rdf:type, D, A2), 
        rdf(C, owl:disjointWith, D, A3), 
        infimum(A1, A2, A3, A). 
