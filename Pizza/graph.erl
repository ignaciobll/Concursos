-module(graph).

-compile(export_all).

-import(lists, [nth/2]).

ady(Pos, {N, Gen}) ->
    Len = length(Gen),
    #{Pos =>
          {case (Pos < N) or (Pos == N) of
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

%% explore(Pos, {N, Gen}, List) ->    
