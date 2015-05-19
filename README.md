# Anql

The SWI Prolog source code of our prototype. This package has the following structure:

* aRDF.pl - the generic framework 
* aRDFS_rules.rl - the RDF Schema rules (only one currently) to describe how to propagate the annotated values 
* tRDF.pl - the temporal RDF specification 
* fuzzyRDF.pl - the fuzzy RDF specification
* tests - the test files 

## Running the examples

This is a sample Prolog session using SWI Prolog 5.8.0 for running our temporal domain example (Alain Prost) and our fuzzy domain example (Sports Cars).

### Alain Prost 
```
?- [tRDF].
% library(assoc) compiled into assoc 0.00 sec, 12,028 bytes
% library(error) compiled into error 0.00 sec, 9,480 bytes
...

?- ardf:run('tests/alainProst.rdf', mykb).
Reasoning finished, 5 inference steps.
true.

?- listing(mykb:_).

quad('http://example.org/ns#AlainProst', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/ns#SportsCarDriver', [[1980, 1991], [1993, 1993]]).
quad('http://example.org/ns#AlainProst', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/ns#F1Driver', [[1980, 1991], [1993, 1993]]).
...
```
### Sports Cars
```
?- [fuzzyRDF].
% library(assoc) compiled into assoc 0.00 sec, 24,008 bytes
% library(error) compiled into error 0.00 sec, 17,776 bytes
...

?- ardf:run('tests/sportsCars.rdf', mykb).
Reasoning finished, 2 inference steps.
true.

?- listing(mykb:_).

quad('http://example.org/ardf#BMWM3', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/ardf#ExpensiveCar', 0.9).
quad('http://example.org/ardf#audiTT', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/ardf#ExpensiveCar', 0.8).
...
```

# AnQL
AnQL is an extension of the SPARQL query language to query Annotated RDF. The AnQL prototype is currently under development and the partial implementation of the language is available. 
