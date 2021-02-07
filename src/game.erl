-module(game).
-export([
    new_board/1,
    can_stay/1,
    num_players/1,
    attacker_card/1,
    current_turn/1,
    next_turn/1,
    has_player_lost/2,
    check_finished/1,
    hand/2,
    hand_from_others/2,
    get_deck_top_from_others/1,
    choose_attacker_card/1,
    choose_attacker_card/2,
    attack/4,
    stay/1,
    board_to_map/2,
    board_to_map/1
]).

-record(card, {
    % int [0, 24)
    num,
    % bool 伏せられているか
    hidden = true
}).
-record(board, {
    % card array array 各プレイヤーが持つカード
    hands,
    % int list 山札
    deck,
    % int <= length(hands) deckの先頭を使ってattackを行う人
    turn = 1,
    % bool stayができるかどうか
    can_stay = false,
    % card: attacker card; {deck, N} or {hand, HI}
    attacker_card
}).

new_board(NumPlayers) when is_integer(NumPlayers), 2 =< NumPlayers, NumPlayers =< 4 ->
    Nums = shuffle_list(lists:seq(0, 23)),
    {HandsNum, Deck} =
        case NumPlayers of
            2 ->
                [N1, N2, N3, N4, N5, N6, N7, N8 | D] = Nums,
                {[[N1, N2, N3, N4], [N5, N6, N7, N8]], D};
            3 ->
                [N1, N2, N3, N4, N5, N6, N7, N8, N9 | D] = Nums,
                {[[N1, N2, N3], [N4, N5, N6], [N7, N8, N9]], D};
            4 ->
                [N1, N2, N3, N4, N5, N6, N7, N8 | D] = Nums,
                {[[N1, N2], [N3, N4], [N5, N6], [N7, N8]], D}
        end,
    Hands = array:from_list([
        array:from_list([#card{num = N} || N <- lists:sort(HandNum)])
        || HandNum <- HandsNum
    ]),
    #board{hands = Hands, deck = Deck}.

can_stay(Board) -> Board#board.can_stay.

num_players(Board) -> array:size(Board#board.hands).

attacker_card(Board) ->
    case Board#board.attacker_card of
        undefined -> undefined;
        {deck, N} -> {deck, N};
        {hand, HI} -> {hand, HI}
    end.

current_turn(Board) -> Board#board.turn.

next_turn(Board = #board{}) ->
    case check_finished(Board) of
        {finished, _} ->
            throw(game_already_finished);
        not_finished ->
            NumPlayers = num_players(Board),
            Can1 = Board#board.turn rem num_players(Board) + 1,
            Can2 = Can1 rem NumPlayers + 1,
            Can3 = Can2 rem NumPlayers + 1,
            case
                {has_player_lost(Board, Can1), has_player_lost(Board, Can2),
                    has_player_lost(Board, Can3)}
            of
                {false, _, _} -> Can1;
                {true, false, _} -> Can2;
                {true, true, false} -> Can3
            end
    end.

has_player_lost(Board = #board{}, PlayerIndex) ->
    % i.e., all cards of hand are revealed?
    lists:all(fun({_, H}) -> not H end, hand(Board, PlayerIndex)).

check_finished(Board = #board{}) ->
    L = [has_player_lost(Board, PI) || PI <- lists:seq(1, num_players(Board))],
    case util:count_true(L) >= num_players(Board) - 1 of
        false -> not_finished;
        true -> {finished, util:find_first_index(fun(B) -> not B end, L)}
    end.

hand(Board = #board{}, PlayerIndex) when is_integer(PlayerIndex) ->
    L = array:to_list(get_hand(Board, PlayerIndex)),
    lists:map(fun(#card{num = N, hidden = H}) -> {N, H} end, L).

hand_from_others(Board = #board{}, PlayerIndex) when is_integer(PlayerIndex) ->
    H = hand(Board, PlayerIndex),
    lists:map(
        fun({N, Hidden}) ->
            case Hidden of
                % If hidden, only its color is shown.
                true -> {N rem 2, Hidden};
                false -> {N, Hidden}
            end
        end,
        H
    ).

get_deck_top_from_others(#board{deck = Deck}) ->
    case Deck of
        [] -> undefined;
        [N | _] -> N rem 2
    end.

choose_attacker_card(Board = #board{deck = Deck}, HandIndex) ->
    case Deck of
        [N | NewDeck] ->
            % Deck is not empty.
            Board#board{deck = NewDeck, attacker_card = {deck, N}};
        [] ->
            % Deck is empty! Use HandIndex.
            % The chosen card should be hidden.
            C = get_hand(Board, current_turn(Board), HandIndex),
            case C#card.hidden of
                false -> throw(invalid_card_state);
                true -> Board#board{attacker_card = {hand, HandIndex}}
            end
    end.

choose_attacker_card(Board) ->
    choose_attacker_card(Board, undefined).

check_attack(Board, TargetPlayer, TargetIndex, Guess) when
    is_integer(TargetPlayer), is_integer(TargetIndex), is_integer(Guess)
->
    case {Board, TargetPlayer =:= current_turn(Board)} of
        {#board{attacker_card = undefined}, _} ->
            throw(attacker_card_not_set);
        {_, true} ->
            throw(invalid_player_index);
        _ ->
            case get_hand(Board, TargetPlayer, TargetIndex) of
                #card{hidden = false} -> throw(invalid_card_state);
                #card{hidden = true, num = Guess} -> success;
                _ -> failure
            end
    end.

reset_attacker_card(Board = #board{}, Hidden) ->
    Turn = current_turn(Board),
    NewBoard =
        case Board#board.attacker_card of
            {deck, N} ->
                OldHand = array:to_list(get_hand(Board, Turn)),
                NewHand = [#card{num = N, hidden = Hidden} | OldHand],
                SortedNewHand = lists:sort(NewHand),
                set_hand(Board, Turn, array:from_list(SortedNewHand));
            {hand, HI} when Hidden =:= false ->
                reveal_hand(Board, Turn, HI);
            {hand, _} ->
                Board
        end,
    NewBoard#board{
        attacker_card = undefined
    }.

attack(Board, TargetPlayer, TargetIndex, Guess) when
    is_integer(TargetPlayer), is_integer(TargetIndex), is_integer(Guess)
->
    case check_attack(Board, TargetPlayer, TargetIndex, Guess) of
        failure ->
            NewBoard = reset_attacker_card(Board, false),
            {failure, NewBoard#board{
                turn = next_turn(Board),
                can_stay = false
            }};
        success ->
            NewBoard = reveal_hand(Board, TargetPlayer, TargetIndex),
            {success, NewBoard#board{
                can_stay = true
            }}
    end.

stay(Board = #board{can_stay = true}) ->
    NewBoard = reset_attacker_card(Board, true),
    NewBoard#board{
        turn = next_turn(Board),
        can_stay = false
    };
stay(#board{can_stay = false}) ->
    throw(cannot_stay).

board_to_map(Board, PlayerIndex) ->
    Map = #{
        can_stay => can_stay(Board),
        current_turn => current_turn(Board),
        next_turn => next_turn(Board),
        num_players => num_players(Board),
        winner =>
            case check_finished(Board) of
                not_finished -> null;
                {finished, Winner} -> Winner
            end,
        hands => [
            [[N, H] || {N, H} <- hand_from_others(Board, PI)]
            || PI <- lists:seq(1, num_players(Board))
        ],
        deck_top => get_deck_top_from_others(Board),
        attacker_card =>
            case attacker_card(Board) of
                undefined -> null;
                {deck, N} -> [1, N];
                {hand, HI} -> [2, HI]
            end
    },
    case PlayerIndex of
        undefined ->
            Map;
        PI ->
            Map#{
                your_player_index => PI,
                your_hand => [N || {N, _H} <- hand(Board, PI)],
                your_attacker_card_from_deck =>
                    case {current_turn(Board), attacker_card(Board)} of
                        {Turn, {deck, N1}} when Turn =:= PI -> N1;
                        _ -> null
                    end
            }
    end.

board_to_map(Board) ->
    board_to_map(Board, undefined).

%%% private functions
shuffle_list(L) when is_list(L) ->
    % Thanks to: https://stackoverflow.com/a/8820501
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

get_hand(Board = #board{hands = Hands}, PlayerIndex) ->
    case 1 =< PlayerIndex andalso PlayerIndex =< num_players(Board) of
        false ->
            throw(invalid_player_index);
        true ->
            array:get(PlayerIndex - 1, Hands)
    end.

get_hand(Board = #board{}, PlayerIndex, HandIndex) ->
    Hand = get_hand(Board, PlayerIndex),
    case 1 =< HandIndex andalso HandIndex =< array:size(Hand) of
        false -> throw(invalid_hand_index);
        true -> array:get(HandIndex - 1, Hand)
    end.

set_hand(Board = #board{hands = OldHands}, PlayerIndex, NewHand) ->
    NewHands = array:set(PlayerIndex - 1, NewHand, OldHands),
    Board#board{hands = NewHands}.

set_hand(Board = #board{}, PlayerIndex, HandIndex, NewCard) ->
    OldHand = get_hand(Board, PlayerIndex),
    NewHand = array:set(HandIndex - 1, NewCard, OldHand),
    set_hand(Board, PlayerIndex, NewHand).

reveal_hand(Board = #board{}, PlayerIndex, HandIndex) ->
    OldCard = get_hand(Board, PlayerIndex, HandIndex),
    case OldCard of
        #card{hidden = false} ->
            throw(invalid_card_state);
        _ ->
            NewCard = OldCard#card{hidden = false},
            set_hand(Board, PlayerIndex, HandIndex, NewCard)
    end.

card_tuple_to_list({N, H}) -> [N, H];
card_tuple_to_list(undefined) -> null.
