-module(aoc_util).
-export([slurp/1]).

slurp(Filename) ->
    {ok, File} = file:open(Filename, [read, binary, {encoding, utf8}]),
    Lines = slurp(File, []),
    file:close(File),
    Lines.

slurp(File, Lines) ->
    case file:read_line(File) of
        eof -> lists:reverse(Lines);
        {ok, RawLine} ->
            Line = string:trim(RawLine, trailing, "\n"),
            slurp(File, [Line | Lines])
    end.
