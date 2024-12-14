-module(sol).
-compile(export_all).

parse(Input) ->
    {ok, Re} = re:compile("mul\\((\\d+),(\\d+)\\)"),
    case re:run(Input, Re, [{capture, all, list}, global]) of
        nomatch -> [];
        {match, Matches} -> parse_matches(Matches, [])
    end.
parse_matches([], Muls) ->
    lists:reverse(Muls);
parse_matches([[_, A, B] | Rest], Muls) ->
    parse_matches(Rest, [{list_to_integer(A), list_to_integer(B)} | Muls]).

parse_full(Lines) ->
    lists:flatten([parse(Line) || Line <- Lines]).

sol1(Muls) ->
    lists:sum([A * B || {A, B} <- Muls]).
