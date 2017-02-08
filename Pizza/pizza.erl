-module(pizza).

-import(file, [open/2, read_line/1, close/1]).
-import(lists,[foldl/3, reverse/1, splitwith/2, filter/2, flatten/1, map/2, nth/2]).
-import(rand, [uniform/1]).

-compile(export_all).

parser(Name) ->
    {ok, IoDevice} = open(Name, [read]),
    [H|T] = read_file(IoDevice),
    [R, C, Min, Max] = string:tokens(H, "x "),
    #{rows => R, columns => C, min => Min, max => Max, pizza => T}.

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

counter(Input) ->
    #{ pizza := Pizza } = Input,
    {T, M} = match({0,0},flatten(Pizza)),
    Input#{t => T, m => M}.

match({T, M}, []) -> {T,M};
match({T, M}, [84|Rest]) -> match({T + 1 , M}, Rest); % 84 = T
match({T, M}, [77|Rest]) -> match({T , M + 1}, Rest). % 77 = M

ady2(Len, Pos, {N, Gen}) ->
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

ady(Len, Pos, {N, Gen}) ->
    {nth(Pos, Gen),
     case (Pos < N) or (Pos == N) of
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
     end}.


generate_groups(Gen={_N, List}) ->
    Len = length(List),
    foldl(fun(X, Acc) -> maps:merge(ady2(Len, X, Gen), Acc) end, maps:new(), lists:seq(1, Len)).


to_gen(Input) ->
    #{ pizza := Pizza } = Input,
    #{ columns := Col } = Input,
    {N, []} = string:to_integer(Col),
    {N, map(fun (_C) -> rand:uniform(4) end, flatten(Pizza))}.

gen_to_group({N, Gen}) -> n_length_chunks(Gen, N).

n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].
