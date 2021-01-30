-module(util).
-export([find_first_index/2, count_true/1]).

find_first_index(_, [], _) ->
    false;
find_first_index(Pred, [H | T], I) ->
    case Pred(H) of
        true -> I;
        false -> find_first_index(Pred, T, I + 1)
    end.

find_first_index(Pred, L) -> find_first_index(Pred, L, 1).

count_true([], Sum) -> Sum;
count_true([H | T], Sum) when H =:= true -> count_true(T, Sum + 1);
count_true([H | T], Sum) when H =:= false -> count_true(T, Sum).

count_true(L) -> count_true(L, 0).
