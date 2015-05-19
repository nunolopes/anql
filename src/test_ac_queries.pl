run_all_queries :-
     run_all_queries(1, 'jistTests/QueriesResults.txt').

run_all_queries(Max, File) :-
     spo_queries_a(Max, File),
     leaf_name_queries_a(Max, File),
     part_of_queries_a(Max, File),
     project_code_queries_a(Max, File),
     two_pattern_queries_a(Max, File),
     two_pattern_queries2_a(Max, File),
     two_pattern_queries3_a(Max, File),
     three_pattern_queries_a(Max, File),
     three_pattern_queries2_a(Max, File),
     three_pattern_queries3_a(Max, File).
     
run_threep_queries(Max, File) :-
     three_pattern_queries_a(Max, File),
     three_pattern_queries2_a(Max, File),
     three_pattern_queries3_a(Max, File).
     
run_twop_queries(Max, File) :-
     two_pattern_queries_a(Max, File).
     %two_pattern_queries2_a(Max, File),
     %two_pattern_queries3_a(Max, File).
    
% SPO
spo_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream, '\n spo_queries'),
     spo_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream, '\n spo_queries_w'),
     spo_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream, '\n spo_queries_wo'),
     spo_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).
     
spo_queries(Max, Stream) :-
      run_query("select * where { ?s ?p ?o}", Max, Stream).

spo_queries_w(Max, Stream) :-
     run_query("select * where { ?s ?p ?o [[Sabrina Kirrane]]}", Max, Stream).

spo_queries_wo(Max, Stream) :-
     run_query("select * where { ?s ?p ?o [[Nuno Lopes]]}", Max, Stream).

%Leaf Name
leaf_name_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream, '\n leaf_name_queries'),
     leaf_name_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream, '\n leaf_name_queries_w'),
     leaf_name_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream, '\n leaf_name_queries_wo'),
     leaf_name_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).
     
leaf_name_queries(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o}", Max, Stream).

leaf_name_queries_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o [[Administrators]]}", Max, Stream).

leaf_name_queries_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o [[NLopes]]}", Max, Stream).

%Client Code
part_of_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n part_of_queries'),
     part_of_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n part_of_queries_w'),
     part_of_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n part_of_queries_wo'),
     part_of_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

part_of_queries(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o }", Max, Stream).

part_of_queries_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o [[Administrators]]}", Max, Stream).
     
part_of_queries_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o [[NLopes]]}", Max, Stream).

%Project Code
project_code_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n project_code_queries'),
     project_code_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n project_code_queries_w'),
     project_code_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n project_code_queries_wo'),
     project_code_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).
     
project_code_queries(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o}", Max, Stream).
     
project_code_queries_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[BWALSH]]}", Max, Stream).
     
project_code_queries_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[NLopes]]}", Max, Stream).

%Two triple patterns
two_pattern_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n two_pattern_queries '),
     two_pattern_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries_w'),
     two_pattern_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries_wo'),
     two_pattern_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

two_pattern_queries(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where {  ?s ent:LeafName ?o . ?s ent:isPartOf ?o}", Max, Stream).

two_pattern_queries_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where {  ?s ent:LeafName ?o [[Sabrina Kirrane]] . ?s ent:isPartOf ?o [[Sabrina Kirrane]] }", Max, Stream).

two_pattern_queries_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o [[NLopes]] . ?s ent:isPartOf ?o [[NLopes]]  }", Max, Stream).

two_pattern_queries2_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n two_pattern_queries2'),
     two_pattern_queries2(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries2_w'),
     two_pattern_queries2_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries2_wo'),
     two_pattern_queries2_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

two_pattern_queries2(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o . ?o ent:isPartOf ?o2}", Max, Stream).

two_pattern_queries2_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o [[Administrators]] . ?o ent:isPartOf ?o2 [[Administrators]]}", Max, Stream).

two_pattern_queries2_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where {?s ent:isPartOf ?o [[NLopes]] . ?o ent:isPartOf ?o2 [[NLopes]]}", Max, Stream).

two_pattern_queries3_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n two_pattern_queries3'),
     two_pattern_queries3(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries3_w'),
     two_pattern_queries3_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n two_pattern_queries3_wo'),
     two_pattern_queries3_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

two_pattern_queries3(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o . ?s ent:Client_Code ?o2}", Max, Stream).

two_pattern_queries3_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[BWALSH]] . ?s ent:Client_Code ?o2 ?a}", Max, Stream).

two_pattern_queries3_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[NLopes]] . ?s  ent:Client_Code ?o2 [[NLopes]]}", Max, Stream).

 %Three triple patterns
three_pattern_queries_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n three_pattern_queries '),
     three_pattern_queries(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries_w'),
     three_pattern_queries_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries_wo'),
     three_pattern_queries_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

three_pattern_queries(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o . ?s ent:isPartOf ?o . ?o ent:isPartOf ?op }", Max, Stream).

three_pattern_queries_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o [[Sabrina Kirrane]] . ?s ent:isPartOf ?o [[Sabrina Kirrane]] . ?o ent:isPartOf ?op [[Sabrina Kirrane]]}", Max, Stream).

three_pattern_queries_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:LeafName ?o [[NLopes]] . ?s ent:isPartOf ?o [[NLopes]] . ?o ent:isPartOf ?op [[NLopes]]}", Max, Stream).

three_pattern_queries2_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n three_pattern_queries2'),
     three_pattern_queries2(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries2_w'),
     three_pattern_queries2_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries2_wo'),
     three_pattern_queries2_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

three_pattern_queries2(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o . ?o ent:isPartOf ?o2 . ?o2 ent:isPartOf ?o3}", Max, Stream).

three_pattern_queries2_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:isPartOf ?o [[Administrators]] . ?o ent:isPartOf ?o2 [[Administrators]] . ?o2 ent:isPartOf ?o3 [[Administrators]]}", Max, Stream).

three_pattern_queries2_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where {?s ent:isPartOf ?o [[NLopes]] . ?o ent:isPartOf ?o2 [[NLopes]] . ?o2 ent:isPartOf ?o3 [[NLopes]]}", Max, Stream).

three_pattern_queries3_a(Max, File) :-
     open(File, append, Stream, [encoding(utf8)]),
     write(Stream,'\n three_pattern_queries3'),
     three_pattern_queries3(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries3_w'),
     three_pattern_queries3_w(Max, Stream),
     write(Stream, '\n AVERAGE'),
     write(Stream,'\n three_pattern_queries3_wo'),
     three_pattern_queries3_wo(Max, Stream),
     write(Stream, '\n AVERAGE'),
     close(Stream).

three_pattern_queries3(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o . ?s ent:Client_Code ?o2. ?s ent:Employee_Code ?o3}", Max, Stream).

three_pattern_queries3_w(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[BWALSH]] . ?s ent:Client_Code ?o2 [[BWALSH]] . ?s ent:Employee_Code ?o3 [[BWALSH]]}", Max, Stream).

three_pattern_queries3_wo(Max, Stream) :-
     run_query("PREFIX ent: <http://urq.deri.org/enterprise#> select * where { ?s ent:Project_Code ?o [[NLopes]] . ?s  ent:Client_Code ?o2 [[NLopes]] . ?s ent:Employee_Code ?o3 [[NLopes]]}", Max, Stream).

% run_query(Query, Max, Stream) :-
%       get_time(S),
%       findall(X, sparql:sparql_query(Query, X, _), L),
%       length(L, Len),
%       get_time(F),
%       R is F - S,
%       format('\n'),
%       write(Len),
%       format('\t'),
%       write(R),
%       Max2 is Max - 1,
%       Max2 > 0 -> run_query(Query, Max2) ; !.

% run_query(Query, Max) :-
%       get_time(S),
%       findall(X, sparql:sparql_query(Query, X, _), L),
%       length(L, Len),
%       get_time(F),
%       R is F - S,
%       open('jistTests/QueriesResults.txt', append, Stream, [encoding(utf8)]),
%       nl(Stream),
%       writeq(Stream, Len),
%       tab(Stream,10),
%       writeq(Stream, R),
%       close(Stream),
%       Max2 is Max - 1,
%       Max2 > 0 -> run_query(Query, Max2) ; !.
      
 run_query(Query, Max, Stream) :-
      get_time(S),
      findall(X, sparql:sparql_query(Query, X, _), L),
      length(L, Len),
      get_time(F),
      R is F - S,
      nl(Stream),
      writeq(Stream, Len),
      writeq(Stream, '*'),
      writeq(Stream, R),
      Max2 is Max - 1,
      Max2 > 0 -> run_query(Query, Max2, Stream) ; !.
      