-module(sol).
-compile(export_all).

parse(Lines) ->
    [
        [list_to_integer(Num) || Num <- string:lexemes(Line, " ")]
        || Line <- Lines
    ].

is_safe(List) ->
    Diffs = [A - B || {A, B} <- lists:zip(List, tl(List), trim)],
    SameSign =
        lists:all(fun (X) -> X > 0 end, Diffs)
        or
        lists:all(fun (X) -> X < 0 end, Diffs),
    SafeDistances = lists:all(fun
        (X) when abs(X) >= 1, abs(X) =< 3 -> true;
        (_) -> false
    end, Diffs),
    SameSign and SafeDistances.

sol1(Reports) ->
    support:count(fun is_safe/1, Reports).

variations(List) ->
    lists:map(fun (N) ->
        {A, B} = lists:split(N, List),
        A ++ tl(B)
    end, lists:seq(0, length(List) - 1)).

is_safe_2(List) ->
    is_safe(List) orelse lists:any(fun is_safe/1, variations(List)).

sol2(Reports) ->
    support:count(fun is_safe_2/1, Reports).
