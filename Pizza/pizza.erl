-module(pizza).

-import(file,  [open/2, read_line/1, close/1]).
-import(rand,  [uniform/1]).
-import(lists, [foldl/3, reverse/1, splitwith/2, filter/2, flatten/1, map/2, nth/2, seq/2, zip/2]).
-import(graph, [generate_ady/1, get_groups/2, analyze_groups/2]).

-compile(export_all).

% Reads input (Google Hash Code format) and translate it into a map.
parser(Name) ->
    {ok, IoDevice} = open(Name, [read]),
    [H|T] = read_file(IoDevice),
    [R, C, Min, Max] = string:tokens(H, "x "),
    #{rows => R, columns => C, min => Min, max => Max, pizza => flatten(T)}.

read_file(IoDevice) ->
    case read_line(IoDevice) of
        {ok, Data} ->
            [_|T] = reverse(Data),
            [reverse(T)|read_file(IoDevice)];
        {error, Reason} ->
            close(IoDevice),
            Reason;
        eof ->
            []
    end.

%Number of Tomatoes (T) ond Mushrooms (M) that are in a pizza.
counter(Input) ->
    #{ pizza := Pizza } = Input,
    {T, M} = match({0,0}, Pizza),
    Input#{t => T, m => M}.

match({T, M}, []) -> {T,M};
match({T, M}, [84|Rest]) -> match({T + 1 , M}, Rest); % 84 = T
match({T, M}, [77|Rest]) -> match({T , M + 1}, Rest). % 77 = M

% Translates a Pizza into a single gen (array) of random groups (1 to 4)-
to_gen(Input) ->
    #{ pizza := Pizza, columns := Col } = Input,
    {N, []} = string:to_integer(Col),
    {N, map(fun (_C) -> rand:uniform(4) end, flatten(Pizza))}.

% Visualize the array as a 2-dimensional array similar to original pizza.
gen_to_group({N, Gen}) -> n_length_chunks(Gen, N).

% Auxiliary method to divide a List into Chunks of n-lenth
n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].

main() ->
    SmallInput = parser("small.in"),
    %% MediumInput = parser("medium.in"),
    %% BigInpun = parser("big.in"),
    SmallGenList = foldl(fun(_, Acc) -> [to_gen(SmallInput)|Acc] end, [], seq(1, 8)),
    SmallAdyList = map(fun(X) -> generate_ady(X) end, SmallGenList),
    SmallGroupsList = map(fun({Ady, Gen}) -> get_groups(Ady, Gen) end, zip(SmallAdyList, SmallGenList)),
    SmallGroupResultList = map(fun({Groups, Gen}) -> analyze_groups(Groups, Gen) end, zip(SmallGroupsList, SmallGenList)),
    SmallGroupResultList.			%here we return the number of well formed groups in the 8 gens
