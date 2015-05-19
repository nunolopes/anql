
% ------------------------------------------------------------------------
% main processing from raw entries
main:-
        get_entries(Res),
        output_triples(Res).


get_entries(Res):-
        [locations],
        findall(
                recordElm(Tag,TimeStamp,IPsF, Min),
                ( setof(ip_ssi(IP,SSI), Seq^entry(TimeStamp,Seq,IP,Tag,SSI), IPs),  filter_ssi(IPs, IPsF, Min)),
                Records
               ),
        sort(Records, RecordsR),
        process_records(RecordsR, Res).


% filters the list of ips to contain only the one with best (lowest) ssi
filter_ssi(IPs, IPsF, Min):-
        filter_ssi(IPs, 256, [], IPsF, Min).

filter_ssi([], Min, ResR, Res, Min):- reverse(ResR, Res).
filter_ssi([ip_ssi(_IP, SSI)|IP_SSIs], Min, It, Res, MinF) :- SSI > Min, !,
        filter_ssi(IP_SSIs, Min, It, Res, MinF).
filter_ssi([ip_ssi(IP, SSI)|IP_SSIs], Min, It, Res, MinF) :- SSI = Min, !,
        filter_ssi(IP_SSIs, Min, [IP|It], Res, MinF).
filter_ssi([ip_ssi(IP, SSI)|IP_SSIs], Min, _It, Res, MinF) :- SSI < Min, 
        filter_ssi(IP_SSIs, SSI, [IP], Res, MinF).


% ------------------------------------------------------------------------
% take data from sql, does not filter over ssi's so we have more triples
main_records:-
        get_records(Res),
        output_triples(Res).


get_records(Res):-
        [recordsInterval],
%        findall(recordElm(Tag,TimeStamp,IPsS,0), (record(Tag,IPs,TimeStamp), sort(IPs, IPsS)), Records),
%        sort(Records, RecordsR),
        records(RecordsR),
        process_records(RecordsR, Res).



% --------------------------------------------------------------------
% group all the timestamps as intervals
process_records(RecordsR, Res):-
%        save('recordsInterval.pl', RecordsR).

        process(RecordsR, RecordsRT),

        remove_low_trusted(RecordsRT, 0.4, Res).



% ---------------------------------------------------------------------------------------------------------------
% auxiliary to setup the list walker, having the first element has staring point. Makes sure it's not empty list.
process([], []).              
process([recordElm(Tag, TS, IP, SSI)|RecordList], Res) :-
        process(RecordList, recordInterval(Tag, IP, TS, _, [SSI]), Res).


% walk trough the records, if the next has the same tag and IP range, store the timepoing as the new end of the
% interval.  If something changes, store the interval and start a new interval with the timepoint as start of the
% interval
%
% process(+RecordList, +Start, -Res).
process([], Record, [RecordF]) :- check_record(Record, RecordF).
process([R1|Records], RTemp, Res):-
        R1 = recordElm(Tag, End, IP, SSIr),
        RTemp = recordInterval(Tag, IP, Start, _, SSIL), !, 
        process(Records, recordInterval(Tag, IP, Start, End, [SSIr|SSIL]), Res).
process([R1|Records], Record, [RecordF|Res]) :-
        check_record(Record, RecordF),
        R1 = recordElm(Tag, Start, IP, SSI),  
        process(Records, recordInterval(Tag, IP, Start, _, [SSI]), Res).

% if record is only a timepoint, instantiate the endpoint and calculate the trust value based on the SSI
check_record(recordInterval(Tag, IP, Start, End, SSIL), recordInterval(Tag, IP, Start, End, TrustValue)) :- (var(End) ->
End = Start; true), trust(SSIL, TrustValue).


% compute the trust based on a list of ssi.  Value is 1 minus the Average of the list divided by the possible maximum of
% the list.
trust(List, Value) :-
        sumlist(List, Sum),
        length(List, Length),
        Avg is Sum / Length,
        Max is 256 * Length,
        ValueR is 1 - (Avg / Max),
        (integer(ValueR) -> W = '~d'; W = '~4f'),
        format(atom(ValueA), W, [ValueR]),
        atom_number(ValueA, Value).


% ---------------------------------------------------------------------------------------------------------------
% remove elementents with low trust
% BUG: need to look for the first one with is above Threshold
remove_low_trusted([Record|Records], Threshold, Res) :-
%        Record = recordInterval(_Tag, _IPs, _Start, _End, Trust),
        remove_low_trusted(Records, Threshold, Record,  Res).
        


remove_low_trusted([], _, Record, [Record]).
remove_low_trusted([R|Records], Threshold, Record, Res):-
        R = recordInterval(_Tag, _IPs, _Start, _End, Trust),
        Trust < Threshold, !,
        remove_low_trusted(Records, Threshold, Record, Res).
remove_low_trusted([R|Records], Threshold, Record, Res):-  % same tag and IPs -> merge Start, End and Trust
        R      = recordInterval(Tag, IPs, _Int2, End,   _Trust2),
        Record = recordInterval(Tag, IPs, Start, _Int1, _Trust1), !,
        aggregTrust(R, Record, TrustF), 
        remove_low_trusted(Records, Threshold, recordInterval(Tag, IPs, Start, End, TrustF), Res).
remove_low_trusted([R|Records], Threshold, Record, [Record|Res]):-  % different Tag or IPs -> store Record
        remove_low_trusted(Records, Threshold, R, Res).
        

aggregTrust(R1, R2, TrustF):-
        R1 = recordInterval(_TR1, _IPsR1, SR1, ER1, TR1),
        IR1 is max(ER1 - SR1, 1),
        R2 = recordInterval(_TR2, _IPsR2, SR2, ER2, TR2), 
        IR2 is max(ER2 - SR2, 1),
        TrustF is (IR1*TR1 + IR2*TR2)/(IR1 + IR2).


% -----------------------------------------------------------
% calculate a graph representing the access between rooms
gen_graph:-
        get_entries(Res),
        generate_graph(Res, Graph), 
        write_dot_file(Graph, 'connections.dot').
        

generate_graph([], Graph) :-
        findall(connection(A,B), connection(A,B), L),
        msort(L, LS),
        LS=[connection(A,B)|LLS],
        new_graph(LLS, connection(1,A,B), Graph),
        retractall(connection(_,_)).
generate_graph([R|Records], Graph):-
        R = recordInterval(_Tag, IPs, _Start, _End, _Trust),
        add_connections(IPs), 
        generate_graph(Records, Graph).


% -----------------------------------------------------------
% assert the connections
add_connections([]).
add_connections([A|IPs]):-
        add_connection(IPs, A),
        add_connections(IPs).

add_connection([], _).
add_connection([B|Bs], A):-
        asserta(connection(A,B)),
        add_connection(Bs, A).


% --------------------------------------------------------------------
% create a graph that aggregates the number of connections between IPs
new_graph([], _, []).
new_graph([connection(A,B)|Ls], connection(It, A, B), Res):- !,
        It1 is It + 1,
        new_graph(Ls, connection(It1, A, B), Res).
new_graph([connection(A,B)|Ls], Elm, [Elm|Res]):-
        new_graph(Ls, connection(1, A,B), Res).


% -----------------------------------------------------------
% create the dot file
write_dot_file(Graph,File):-
        empty_assoc(A),
        replace_ids(Graph, 0, A, IDs, GraphID),

        open(File, write, FileStream),
        current_output(Stream),
        set_output(FileStream),
        write('graph {'), nl,
        maplist(print_ids, IDs),
        maplist(print_edges, GraphID),
        write('}'), nl,

        set_output(Stream),
        close(FileStream).

print_ids(IP-ID) :- format('n~w [label="~w"];\n', [ID, IP]).

print_edges(connection(Total, _, _)):- Total < 500, !.
print_edges(connection(Total, A, B)) :-
        Color is round(255 * (1/log(Total/50))),
        format_hex(Color, Hex),
%         Color2 is 255 - Color,
%         format_hex(Color2, Hex2),
        format('  n~w -- n~w [color="#~w~w~w"]\n', [A, B, Hex, Hex, Hex]).

format_hex(Color, Hex) :- Color < 16, !, 
        format(atom(Hex), '0~16r', [Color]).
format_hex(Color, Hex) :- 
        format(atom(Hex), '~16r', [Color]).


% --------------------------------
% replace the IPs with a unique ID
replace_ids([], _, Assoc, AssocL, []):- assoc_to_list(Assoc, AssocL).
replace_ids([connection(Total, A, B)|Connections], It, AssocIt, Assoc, [connection(Total, AId, BId)|ConnectionsIDs]):-
        get_id(A, It, AssocIt, It1, AId, NewAssoc1),
        get_id(B, It1, NewAssoc1, It2, BId, NewAssoc2),
        replace_ids(Connections, It2, NewAssoc2, Assoc, ConnectionsIDs).

get_id(IP, It, Assoc, It, IPId, Assoc):- get_assoc(IP, Assoc, IPId), !.
get_id(IP, It, Assoc, NewIt, It, NewAssoc):- put_assoc(IP, Assoc, It, NewAssoc), NewIt is It + 1.




% -----------------------------------------------------------
% format the list of triples according to a predifined output
output_triples(Res):-
        maplist(print_record, Res), % output triples
        
%        length(RecordsR, RecordsRL),
        length(Res, ResL),
        format('Triples: ~w\n', [ResL]).


print_record(recordInterval(Tag, IPs, Start, End, Trust)) :-
        concat_atom(IPs, '; ',IPsA),
        format_date_from_timestamp(Start, StartDate),
        format_date_from_timestamp(End, EndDate),
        format(':tag~w :locatedIn "~w" . ([~w, ~w],~w)\n', [Tag,IPsA,StartDate, EndDate, Trust]).

format_date_from_timestamp(Timestamp, Date):-
%         Time is Timestamp/1000,  
        format_time(atom(Date), '"%FT%TZ"', Timestamp).


% -------------------------------------
% save data into a file

save(Goal, List, File):-
        open(File, write, FileStream),
        current_output(Stream),
        set_output(FileStream),
        
        maplist(Goal, List),
     
        set_output(Stream),
        close(FileStream).


% save(File, List):-
%         open(File, write, FileStream),
%         current_output(Stream),
%         set_output(FileStream),
%         write('records('), writeq(List), write(').'),
%         set_output(Stream),
%         close(FileStream).
