-module(support).
-export([slurp/1, input/1]).

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
