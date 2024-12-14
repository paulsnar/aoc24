-module(sol).
-compile(export_all).

parse(Lines) ->
    parse_rules(Lines, digraph:new()).

parse_rules(["" | Rest], Graph) ->
    parse_updates(Rest, [], Graph);
parse_rules([Rule | Rest], G) ->
    [A, B] = [list_to_integer(X) || X <- string:lexemes(Rule, "|")],
    digraph:add_vertex(G, A),
    digraph:add_vertex(G, B),
    ['$e'|_] = digraph:add_edge(G, A, B),
    parse_rules(Rest, G).

parse_updates([], Updates, Graph) ->
    {Graph, lists:reverse(Updates)};
parse_updates([Line | Rest], Updates, Graph) ->
    Upd = [list_to_integer(X) || X <- string:lexemes(Line, ",")],
    parse_updates(Rest, [Upd | Updates], Graph).

splits(List) ->
    lists:map(fun (N) ->
        {A, B} = lists:split(N, List),
        {A, hd(B), tl(B)}
    end, lists:seq(0, length(List) - 1)).

is_valid([], _) ->
    true;
is_valid([Page | Rest], Graph) ->
    Pre = digraph:in_neighbours(Graph, Page),
    case lists:any(fun (X) -> lists:member(X, Pre) end, Rest) of
        true -> false;
        false -> is_valid(Rest, Graph)
    end.

mid(Update) ->
    Mid = ceil(length(Update) / 2),
    lists:nth(Mid, Update).

sol1(Graph, Updates) ->
    Mids = lists:filtermap(fun (Update) ->
        case is_valid(Update, Graph) of
            true -> {true, mid(Update)};
            false -> false
        end
    end, Updates),
    lists:sum(Mids).

sol2(Graph, Updates) ->
    Invalid = lists:filter(fun (Update) -> not is_valid(Update, Graph) end, Updates),
    Fixed = [fix(Update, Graph) || Update <- Invalid],
    Mids = [mid(Update) || Update <- Fixed],
    lists:sum(Mids).

has_edge(A, B, Graph) ->
    lists:member(B, digraph:out_neighbours(Graph, A)).

fix(Update, Graph) ->
    lists:sort(fun (A, B) -> has_edge(A, B, Graph) end, Update).

