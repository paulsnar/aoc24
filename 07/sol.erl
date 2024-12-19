-module(sol).
-compile(export_all).

eval(Args, Ops) ->
    [Start | Rest] = Args,
    eval(Start, Rest, Ops).

eval(Acc, [], []) ->
    Acc;
eval(Acc, [Arg | RestArgs], [Op | RestOps]) ->
    Res = op(Acc, Arg, Op),
    eval(Res, RestArgs, RestOps).

op(A, B, concat) ->
    list_to_integer(
        integer_to_list(A)
        ++ integer_to_list(B)
    );
op(A, B, add) -> A + B;
op(A, B, mul) -> A * B.

bit_to_op(0) -> add;
bit_to_op(1) -> mul.
bits_to_ops(N, Bits) ->
    bits_to_ops(N, Bits, []).
bits_to_ops(0, 0, Ops) -> lists:reverse(Ops);
bits_to_ops(N, Bits, Ops) ->
    Op = bit_to_op(Bits band 1),
    bits_to_ops(N - 1, Bits bsr 1, [Op | Ops]).

permute(N) ->
    permute(N, 1 bsl N - 1).
permute(_, -1) ->
    undefined;
permute(N, Acc) ->
    Next = fun () -> permute(N, Acc - 1) end,
    {bits_to_ops(N, Acc), Next}.

permute_3(N) ->
    permute_3(N, floor(math:pow(3, N)) - 1).
permute_3(_, -1) ->
    undefined;
permute_3(N, Acc) ->
    Next = fun() -> permute_3(N, Acc - 1) end,
    {threes_to_ops(N, Acc), Next}.

threes_to_ops(N, Threes) ->
    Ops = [
        case Pos of
            $0 -> add;
            $1 -> mul;
            $2 -> concat
        end
        || Pos <- integer_to_list(Threes, 3)
    ],
    lists:duplicate(N - length(Ops), add) ++ Ops.

search(Result, Args, Generator) ->
    {Ops0, Next0} = Generator(length(Args) - 1),
    search(Result, Args, Ops0, Next0).
search(Result, Args, Ops, NextOps) ->
    EvalResult = eval(Args, Ops),
    case Result =:= EvalResult of
        true -> {ok, Ops};
        false -> case NextOps() of
            {Ops1, NextOps1} -> search(Result, Args, Ops1, NextOps1);
            undefined -> undefined
        end
    end.

parse(Lines) ->
    parse(Lines, []).
parse([], Eqs) ->
    lists:reverse(Eqs);
parse([Line | Rest], Eqs) ->
    [Result | Args] = [list_to_integer(X) || X <- string:lexemes(Line, ": ")],
    parse(Rest, [{Result, Args} | Eqs]).

sol1(Eqs) ->
    lists:foldl(fun ({Result, Args}, Sum) ->
        case search(Result, Args, fun permute/1) of
            {ok, _} -> Sum + Result;
            undefined -> Sum
        end
    end, 0, Eqs).

sol2(Eqs) ->
    lists:foldl(fun ({Result, Args}, Sum) ->
        case search(Result, Args, fun permute_3/1) of
            {ok, _} -> Sum + Result;
            undefined -> Sum
        end
    end, 0, Eqs).
