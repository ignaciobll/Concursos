-module(pizza).
-import(file, [open/2, read_line/1, close/1]).
-import(lists,[foldl/3, reverse/1, splitwith/2, filter/2, flatten/1]).
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

