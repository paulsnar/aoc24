-module(sol).
-compile(export_all).

parse(Input) ->
    lists:foldl(
        fun (Line, {As, Bs}) ->
            [Num1, Num2] = [list_to_integer(X) || X <- string:lexemes(Line, " ")],
            {[Num1 | As], [Num2 | Bs]}
        end,
        {[], []},
        Input
    ).

sol1(As, Bs) ->
    An = lists:sort(As),
    Bn = lists:sort(Bs),
    Diffs = [abs(A - B) || {A, B} <- lists:zip(An, Bn)],
    lists:sum(Diffs).

sol2(As, Bs) ->
    Counts = lists:foldl(
        fun (Num, Counts) ->
            maps:put(Num, maps:get(Num, Counts, 0) + 1, Counts)
        end,
        maps:new(),
        Bs
    ),
    lists:foldl(
        fun (Num, Total) ->
            Count = maps:get(Num, Counts, 0),
            Total + Num * Count
        end,
        0,
        As
    ).
