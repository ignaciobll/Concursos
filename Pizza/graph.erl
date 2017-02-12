-module(graph).

-import(lists, [nth/2, member/2, flatten/1, foldl/3, sort/1, seq/2, map/2]).

-compile(export_all).

%% function that returns a map with the adyacence relation
generate_ady(Gen={_N, List}) ->
    Len = length(List),
    foldl(fun(X, Acc) -> maps:merge(ady(Len, X, Gen), Acc) end, maps:new(), seq(1, Len)).

%% functions that returns a map key -> position, value -> adyacent nodes
                                                % Group values of adyacent nodes. NODE => {UP, RIGHT, LEFT, DOWN}
ady(Len, Pos, {N, Gen}) ->
    #{Pos =>
          {case (Pos < N) or (Pos == N) of
               true  -> b;
               false -> nth(Pos - N, Gen)
           end,
           case Pos rem N == 0 of
               true  -> b;
               false -> nth(Pos + 1, Gen)
           end,
           case Pos rem N == 1 of
               true  -> b;
               false -> nth(Pos - 1, Gen)
           end,
           case Pos > Len - N of
               true -> b;
               false -> nth(Pos + N, Gen)
           end}
     }.

%% functions that searchs for a adyacent group
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
    clean_group(flatten(Res)).

%% function that takes a group, deletes the repeated elements and sorts it
clean_group(List) -> sort(sets:to_list(sets:from_list(List))).

%% function that takes all the groups from a gen
get_groups(Ady, G={_N, Gen}) ->
    {_, Groups} = foldl(fun(X, {Visited, Acc}) ->
                                case member(X, Visited) of
                                    true  -> {Visited, Acc};
                                    false ->
                                        Group = get_group(Ady, X, [], G),
                                        {Group ++ Visited, [Group|Acc]}
                                end
                        end, {[], []}, seq(1, length(Gen))),
    Groups.


%% function that checks if a group is well formed (parallelogram)
well_formed(Set, {N, _Gen}) ->
    {TLrow, TLcol} = colrow(lists:min(Set), N),
    {DRrow, DRcol} = colrow(lists:max(Set), N),
    length(Set) == (DRrow - (TLrow - 1)) * (DRcol -(TLcol - 1)).

%% function that returns the number of wel_formed groups of a gen
analyze_groups(Groups, Gen) ->
    foldl(fun(X, Acc) ->
                  case well_formed(X, Gen) of
                      true  -> 1 + Acc;
                      false -> Acc
                  end
          end, 0, Groups).

%% function that analyze the correct number of ingredients in the given groups
analyze_ingredients(Groups, Input) ->
    #{ pizza := Pizza, min := Min } = Input,
    map(fun(Group) -> minimum_ingredients(Group, Pizza, Min) end, Groups).

%%  function that returns if a grup has the min number of ingredients
minimum_ingredients(Group, Pizza, Min) ->
    {NT, NM} = foldl(fun(X, {T, M}) ->
                             case nth(X, Pizza) of
                                 84 -> {T + 1, M};         % 84 == T
                                 77 -> {T, M + 1}          % 77 == M
                             end
                     end, {0, 0}, Group),
    (NT >= Min) and (NM >= Min).

colrow(Pos, N) -> { (Pos div N) + 1, ((Pos-1) rem N) + 1}. % {row, col}

%% auxiliar function that returns the color of a node
color_of_node(Node, {_N, Gen}) -> nth(Node, Gen).

%%                                                 % List of nodes in Gen with group Group
%% nodes_of_color(Color, {N, Gen}) ->
%%     lists:filter(fun (Pos) -> color_of_node(Pos, {N, Gen}) == Color end,
%%                  lists:seq(1, length(Gen))).

%% is_ady(Pos, ady, to, Node, {N, Gen}) ->
%%     #{Pos := TupleAdy } = ady(Pos, {N, Gen}),
%%     lists:member(Node, tuple_to_list(TupleAdy)).


%%                                                 % A set of integer positions.
