-module(game_test).

-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

new_board_test_() ->
    [
        {"new_board creates a valid new board.", ?setup(fun new_board/1)},
        {"scenario1; #players is 2.", ?setup(fun scenario1/1)},
        {"scenario2; #players is 4.", ?setup(fun scenario2/1)}
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
        ?_assertEqual(game:attacker_card(B), undefined),
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

scenario1(_) ->
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
        ?_assertEqual(cards2nums(game:hand(B1, 1)), [4, 10, 18, 21]),
        ?_assertEqual(cards2nums(game:hand(B1, 2)), [1, 15, 17, 23]),
        % correct deck?
        ?_assertMatch(#card{num = N} when N rem 2 =:= 0, game:deck_top(B1)),
        ?_assertMatch(undefined, game:deck_top(B100)),
        % correct attacker card?
        ?_assertMatch({deck, #card{num = 14}}, game:attacker_card(B2)),
        ?_assertMatch({hand, 1}, game:attacker_card(B101)),
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
        % correct ability to stay?
        ?_assertNot(game:can_stay(B1)),
        ?_assertNot(game:can_stay(B2)),
        ?_assertNot(game:can_stay(B3)),
        ?_assertNot(game:can_stay(B4)),
        ?_assertNot(game:can_stay(B7)),
        ?_assert(game:can_stay(B5)),
        ?_assert(game:can_stay(B6)),
        % correct attack?
        ?_assertMatch(#card{hidden = false}, lists:nth(3, game:hand(B3, 1))),
        ?_assertMatch(#card{hidden = false}, lists:nth(5, game:hand(B5, 1))),
        ?_assertMatch(#card{hidden = false}, lists:nth(1, game:hand(B102, 2))),
        % correctly check if the game has finished?
        ?_assertEqual(not_finished, game:check_finished(B105)),
        ?_assertEqual({finished, 1}, game:check_finished(B106)),
        % correctly converted into map?
        ?_assertMatch(
            #{
                attacker_card := [1, [0, true, _]],
                can_stay := false,
                current_turn := 1,
                deck_top := [0, true, _],
                hands := [
                    [[0, true, _], [0, true, _], [0, true, _], [1, true, _]],
                    [[1, true, _], [1, true, _], [1, true, _], [1, true, _]]
                ],
                next_turn := 2,
                num_players := 2,
                winner := null
            },
            game:board_to_map(B2)
        ),
        ?_assertMatch(
            #{
                attacker_card := [1, [0, true, _]],
                can_stay := false,
                current_turn := 1,
                deck_top := [0, true, _],
                hands := [
                    [[0, true, _], [0, true, _], [0, true, _], [1, true, _]],
                    [[1, true, _], [1, true, _], [1, true, _], [1, true, _]]
                ],
                next_turn := 2,
                num_players := 2,
                winner := null,
                your_attacker_card_from_deck := 14,
                your_hand := [4, 10, 18, 21],
                your_player_index := 1
            },
            game:board_to_map(B2, 1)
        )
    ].

scenario2(_) ->
    B1 = game:new_board(4),
    % turn = 1
    B2 = game:choose_attacker_card(B1),
    {success, B3} = game:attack(B2, 2, 1, 18),
    {success, B4} = game:attack(B3, 2, 2, 21),
    {success, B5} = game:attack(B4, 3, 1, 1),
    {success, B6} = game:attack(B5, 3, 2, 17),
    [
        ?_assert(game:has_player_lost(B4, 2)),
        ?_assert(game:has_player_lost(B6, 3)),
        ?_assertEqual(game:next_turn(B4), 3),
        ?_assertEqual(game:next_turn(B6), 4)
    ].

draw_N_times(Board, 0) ->
    Board;
draw_N_times(Board, N) ->
    B1 = game:choose_attacker_card(Board),
    TargetPlayer = game:next_turn(B1),
    TargetHand = game:hand(B1, TargetPlayer),
    case util:find_first_index(fun(C) -> C#card.hidden end, TargetHand) of
        false ->
            erlang:error(invalid_board);
        HandIndex ->
            Guess =
                case lists:nth(HandIndex, TargetHand) of
                    #card{num = 0} -> 1;
                    #card{num = I} -> I - 1
                end,
            {failure, B2} = game:attack(B1, game:next_turn(B1), HandIndex, Guess),
            draw_N_times(B2, N - 1)
    end.

%% Private functions
cards2nums([#card{num = N} | L]) -> [N | cards2nums(L)];
cards2nums([]) -> [].
