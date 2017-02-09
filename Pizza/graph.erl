-module(graph).

-compile(export_all).

-import(lists, [nth/2, map/2, foldl/3]).

ady(Pos, {N, Gen}) ->
    Len = length(Gen),
    #{Pos =>
          {case (Pos =< N) of
               true  -> b;
               false -> Pos - N
           end,
           case Pos rem N == 0 of
               true  -> b;
               false -> Pos + 1
           end,
           case Pos rem N == 1 of
               true  -> b;
               false -> Pos - 1
           end,
           case Pos > Len - N of
               true -> b;
               false -> Pos + N
           end}
     }.

%% explore(Pos, {N, Gen}, []) ->
%%     GPos = nth(Pos, Gen),
%%     GPosNodes = 

group_of_node(Node, {_N, Gen}) -> lists:nth(Node, Gen).

% List of nodes in Gen with group Group
nodes_of_group(Group, {N, Gen}) ->
    lists:filter(fun (Pos) -> group_of_node(Pos, {N, Gen}) == Group end, 
		 lists:seq(1, length(Gen))).

is_ady(Pos, ady, to, Node, {N, Gen}) ->
    #{Pos := TupleAdy } = ady(Pos, {N, Gen}),
    lists:member(Node, tuple_to_list(TupleAdy)).

colrow(Pos, N) -> { (Pos div N) + 1, ((Pos-1) rem N) + 1}. % {row, col}

% A set of integer positions.
well_formed(Set, {N, _Gen}) ->
    {TLrow, TLcol} = colrow(lists:min(Set), N),
    {DRrow, DRcol} = colrow(lists:max(Set), N),
    length(Set) == (DRrow - (TLrow - 1)) * (DRcol -(TLcol - 1)).
