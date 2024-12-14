-module(sol).
-compile(export_all).

-record(grid, {w, h, p}).

parse(Input) ->
    H = length(Input),
    W = length(hd(Input)),
    Grid = lists:foldl(fun ({Line, Y}, Grid1) ->
        lists:foldl(fun ({Pix, X}, Grid2) ->
            maps:put({X, Y}, Pix, Grid2)
        end, Grid1, lists:zip(Line, lists:seq(0, W - 1)))
    end, #{}, lists:zip(Input, lists:seq(0, H - 1))),
    #grid{w=W, h=H, p=Grid}.

variants({X, Y}) ->
    Dirs = [
        {Dx, Dy}
        ||
            Dx <- lists:seq(-1, 1),
            Dy <- lists:seq(-1, 1),
            {Dx, Dy} /= {0, 0}
    ],
    lists:map(fun ({Dx, Dy}) ->
        [
            {X + Dx * N, Y + Dy * N}
            || N <- lists:seq(0, 3)
        ]
    end, Dirs).
valid_variants(Variants, #grid{w=W, h=H}) ->
    lists:filter(fun (Variant) ->
        lists:all(fun ({X, Y}) ->
            support:in_range(X, {0, W - 1}) andalso support:in_range(Y, {0, H - 1})
        end, Variant)
    end, Variants).

index(Poss, #grid{p=Grid}) ->
    [maps:get(Pos, Grid) || Pos <- Poss].

sol1(Grid) ->
    maps:fold(fun
        (Pos, $X, N) ->
            Vars = valid_variants(variants(Pos), Grid),
            Strs = [index(Var, Grid) || Var <- Vars],
            N + support:count(fun
                ("XMAS") -> true;
                (_) -> false
            end, Strs);
        (_, _, N) -> N
    end, 0, Grid#grid.p).

xvariants({X, Y}) ->
    [
        % 1.4
        % .3.
        % 2.5
        [{X - 1, Y - 1}, {X - 1, Y + 1}, {X, Y}, {X + 1, Y - 1}, {X + 1, Y + 1}],

        % 1.2
        % .3.
        % 4.5
        [{X - 1, Y - 1}, {X + 1, Y - 1}, {X, Y}, {X - 1, Y + 1}, {X + 1, Y + 1}],

        % 4.1
        % .3.
        % 5.2
        [{X + 1, Y - 1}, {X + 1, Y + 1}, {X, Y}, {X - 1, Y - 1}, {X - 1, Y + 1}],

        % 4.5
        % .3.
        % 1.2
        [{X - 1, Y + 1}, {X + 1, Y + 1}, {X, Y}, {X - 1, Y - 1}, {X + 1, Y - 1}]
    ].

sol2(Grid) ->
    maps:fold(fun
        (Pos, $A, N) ->
            Vars = valid_variants(xvariants(Pos), Grid),
            Strs = [index(Var, Grid) || Var <- Vars],
            N + support:count(fun
                ("MMASS") -> true;
                (_) -> false
            end, Strs);
        (_, _, N) -> N
    end, 0, Grid#grid.p).
