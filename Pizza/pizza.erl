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


ady(Len, Pos, {N, Gen}) when Pos > (Len - N) ->
    {nth(Pos    , Gen),
     nth(Pos - N, Gen),
     nth(Pos - 1, Gen),
     b};
ady(_Len, Pos, {N, Gen}) when Pos < N->
    {nth(Pos    , Gen),
     b,
     nth(Pos - 1, Gen),
     nth(Pos + 1, Gen),
     nth(Pos + N, Gen)};
ady(_Len, Pos, {N, Gen}) when Pos rem N == 0 ->
    {nth(Pos    , Gen),
     nth(Pos - N, Gen),
     nth(Pos - 1, Gen),
     b,
     nth(Pos + N, Gen)};
ady(_Len, Pos, {N, Gen}) ->
    {nth(Pos    , Gen),
     nth(Pos - N, Gen),
     nth(Pos - 1, Gen),
     nth(Pos + 1, Gen),
     nth(Pos + N, Gen)}.
    

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




