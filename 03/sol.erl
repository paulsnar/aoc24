-module(sol).
-compile(export_all).

parse(Input) ->
    {ok, Re} = re:compile("(do\\(\\)|don't\\(\\))|mul\\((\\d+),(\\d+)\\)"),
    case re:run(Input, Re, [{capture, all, list}, global]) of
        nomatch -> [];
        {match, Matches} -> parse_matches(Matches, [])
    end.
parse_matches([], Instr) ->
    lists:reverse(Instr);
parse_matches([["do()", _] | Rest], Instr) ->
    parse_matches(Rest, [enable | Instr]);
parse_matches([["don't()", _] | Rest], Instr) ->
    parse_matches(Rest, [disable | Instr]);
parse_matches([[_, _, A, B] | Rest], Instr) ->
    parse_matches(Rest, [{mul, list_to_integer(A), list_to_integer(B)} | Instr]).

parse1(Input) ->
    lists:filter(fun
        ({mul, _, _}) -> true;
        (_) -> false
    end, parse(Input)).

parse_full(Lines) ->
    lists:flatten([parse(Line) || Line <- Lines]).

sol1(Instr) ->
    lists:foldl(fun
        ({mul, A, B}, Sum) -> Sum + A * B;
        (_, Sum) -> Sum
    end, 0, Instr).

sol2(Instr) ->
    {_, Sum} = lists:foldl(fun
        ({mul, _, _}, {false, Sum}) -> {false, Sum};
        ({mul, A, B}, {true, Sum}) -> {true, Sum + A * B};
        (enable, {_, Sum}) -> {true, Sum};
        (disable, {_, Sum}) -> {false, Sum}
    end, {true, 0}, Instr),
    Sum.
