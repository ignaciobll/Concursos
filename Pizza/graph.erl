-module(graph).

-compile(export_all).

-import(lists, [nth/2, member/2, flatten/1, foldl/3, sort/1, seq/2]).

-import(sets, [add_element/2, from_list/1, to_list/1]).

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

explore(Pos, N, Color, Visited, {Color, _, _, _}, Gen, Ady) ->
    [(Pos - N) |
     case member(Pos - N, Visited) of
         true  -> [];
         false -> get_group(Ady, Pos - N, Visited, Gen)
     end];
explore(Pos, _, Color, Visited, {_, Color, _, _}, Gen, Ady) ->
    [(Pos + 1) |
     case member(Pos + 1, Visited) of
         true  -> [];
         false -> get_group(Ady, Pos + 1, Visited, Gen)
     end];
explore(Pos, _, Color, Visited, {_, _, Color, _}, Gen, Ady) ->
    [(Pos - 1) |
     case member(Pos - 1, Visited) of
         true  -> [];
         false -> get_group(Ady, Pos - 1, Visited, Gen)
     end];
explore(Pos, N, Color, Visited, {_, _, _, Color}, Gen, Ady) ->
    [(Pos + N) |
     case member(Pos + N, Visited) of
         true  -> [];
         false -> get_group(Ady, Pos + N, Visited, Gen)
     end];
explore(_, _, _ ,_ , _, _, _) -> [].

get_group(Ady, Pos, Visited, G={N, _Gen}) ->
    Color = color_of_node(Pos, G),
    #{ Pos := {Up, Right, Left, Down} } = Ady,
    Res =
        [ Pos
          | [explore(Pos, N, Color, [Pos|Visited], {Up, nop, nop, nop}, G, Ady)
             | [explore(Pos, N, Color, [Pos|Visited], {nop, Right, nop, nop}, G, Ady)
                | [explore(Pos, N, Color, [Pos|Visited], {nop, nop, Left, nop}, G, Ady)
                   | [explore(Pos, N, Color, [Pos|Visited], {nop, nop, nop, Down}, G, Ady)
                      | []]]]]],
    clean_list(flatten(Res)).

clean_list(List) -> sort(to_list(from_list(List))).

get_groups(Ady, G={_N, Gen}) ->
    foldl(fun(X, Acc) -> [get_group(Ady, X, [], G)|Acc] end, [], seq(1, length(Gen))).

%% explore(Pos, {N, Gen}, []) ->
%%     GPos = nth(Pos, Gen),
%%     GPosNodes =

color_of_node(Node, {_N, Gen}) -> lists:nth(Node, Gen).

                                                % List of nodes in Gen with group Group
nodes_of_color(Color, {N, Gen}) ->
    lists:filter(fun (Pos) -> color_of_node(Pos, {N, Gen}) == Color end,
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

