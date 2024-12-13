-module(support).
-export([slurp/1, input/1, count/2]).

-spec slurp(iodata()) -> list(string()).
slurp(Filename) ->
    {ok, File} = file:open(Filename, [read, {encoding, utf8}]),
    Lines = slurp(File, []),
    file:close(File),
    Lines.

-spec slurp(any(), list(string())) -> list(string()).
slurp(File, Lines) ->
    case file:read_line(File) of
        eof -> lists:reverse(Lines);
        {ok, RawLine} ->
            Line = string:trim(RawLine, trailing, "\n"),
            slurp(File, [Line | Lines])
    end.

-spec input(string()) -> list(string()).
input(Text) ->
    string:split(Text, "\n", all).

-spec count(fun((X) -> boolean()), list(X)) -> integer().
count(Predicate, List) ->
    lists:foldl(fun (X, Sum) ->
        case Predicate(X) of
            true -> Sum + 1;
            false -> Sum
        end
    end, 0, List).
