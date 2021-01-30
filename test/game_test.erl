-module(game_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

new_board_test_() ->
    [
        {"new_board creates a valid new board", ?setup(fun new_board/1)},
        {"everything works file", ?setup(fun everything_works_fine/1)}
    ].

start() ->
    rand:seed(exsss, 0),
    ok.

stop(_) ->
    ok.

new_board(_) ->
    [new_board_N(2), new_board_N(3), new_board_N(4)].

new_board_N(N) ->
    B = game:new_board(N),
    Hands = [game:hand(B, PlayerIndex) || PlayerIndex <- lists:seq(1, game:num_players(B))],
    [
        ?_assertEqual(game:num_players(B), N),
        ?_assertEqual(game:current_turn(B), 1),
        ?_assertEqual(game:next_turn(B), 2),
        ?_assert(
            % Correct numer of hands
            lists:all(
                fun(H) ->
                    length(H) =:=
                        case N of
                            2 -> 4;
                            3 -> 3;
                            4 -> 2
                        end
                end,
                Hands
            )
        )
    ].

everything_works_fine(_) ->
    B1 = game:new_board(2),
    % turn = 1
    B2 = game:choose_attacker_card(B1),
    {failure, B3} = game:attack(B2, 2, 1, 0),
    % turn = 2
    B4 = game:choose_attacker_card(B3),
    {success, B5} = game:attack(B4, 1, 5, 21),
    {success, B6} = game:attack(B5, 1, 4, 18),
    B7 = game:stay(B6),
    % turn = 1
    B100 = draw_N_times(B7, 14),
    B101 = game:choose_attacker_card(B100, 1),
    {success, B102} = game:attack(B101, 2, 1, 0),
    {success, B103} = game:attack(B102, 2, 2, 1),
    {success, B104} = game:attack(B103, 2, 9, 15),
    {success, B105} = game:attack(B104, 2, 10, 17),
    {success, B106} = game:attack(B105, 2, 12, 23),
    [
        % correct hand?
        ?_assertEqual(game:hand(B1, 1), [{4, true}, {10, true}, {18, true}, {21, true}]),
        ?_assertEqual(game:hand(B1, 2), [{1, true}, {15, true}, {17, true}, {23, true}]),
        % correct deck?
        ?_assertEqual(game:get_deck_top_from_others(B1), {0, true}),
        ?_assertEqual(game:get_deck_top_from_others(B100), none),
        % correct turn?
        ?_assertEqual(game:current_turn(B1), 1),
        ?_assertEqual(game:current_turn(B2), 1),
        ?_assertEqual(game:current_turn(B3), 2),
        ?_assertEqual(game:current_turn(B4), 2),
        ?_assertEqual(game:current_turn(B5), 2),
        ?_assertEqual(game:current_turn(B6), 2),
        ?_assertEqual(game:current_turn(B7), 1),
        ?_assertEqual(game:current_turn(B100), 1),
        % correct next turn?
        ?_assertEqual(game:next_turn(B1), 2),
        ?_assertEqual(game:next_turn(B2), 2),
        ?_assertEqual(game:next_turn(B3), 1),
        ?_assertEqual(game:next_turn(B4), 1),
        ?_assertEqual(game:next_turn(B5), 1),
        ?_assertEqual(game:next_turn(B6), 1),
        ?_assertEqual(game:next_turn(B7), 2),
        % correct hand shown?
        ?_assertEqual(
            game:hand_from_others(B3, 1),
            [{0, true}, {0, true}, {14, false}, {0, true}, {1, true}]
        ),
        % correct ability to stay?
        ?_assertNot(game:can_stay(B1)),
        ?_assertNot(game:can_stay(B2)),
        ?_assertNot(game:can_stay(B3)),
        ?_assertNot(game:can_stay(B4)),
        ?_assertNot(game:can_stay(B7)),
        ?_assert(game:can_stay(B5)),
        ?_assert(game:can_stay(B6)),
        % correctly check if the game has finished?
        ?_assertEqual(game:check_finished(B105), not_finished),
        ?_assertEqual(game:check_finished(B106), {finished, 1})
    ].

draw_N_times(Board, 0) ->
    Board;
draw_N_times(Board, N) ->
    B1 = game:choose_attacker_card(Board),
    TargetPlayer = game:next_turn(B1),
    TargetHand = game:hand(B1, TargetPlayer),
    case util:find_first_index(fun({_, H}) -> H end, TargetHand) of
        false ->
            erlang:error(invalid_board);
        HandIndex ->
            Guess =
                case lists:nth(HandIndex, TargetHand) of
                    {0, _} -> 1;
                    {I, _} -> I - 1
                end,
            {failure, B2} = game:attack(B1, game:next_turn(B1), HandIndex, Guess),
            draw_N_times(B2, N - 1)
    end.
