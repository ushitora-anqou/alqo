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
    stay/1
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
    % card list 山札
    deck,
    % int <= length(hands) deckの先頭を使ってattackを行う人
    turn = 1,
    % bool stayができるかどうか
    can_stay = false,
    % card: attacker card; {deck, C} or {hand, HI}
    attacker_card
}).

new_board(NumPlayers) when is_integer(NumPlayers), 2 =< NumPlayers, NumPlayers =< 4 ->
    Cards = [#card{num = N} || N <- shuffle_list(lists:seq(0, 23))],
    {Hands, Deck} =
        case NumPlayers of
            2 ->
                [C1, C2, C3, C4, C5, C6, C7, C8 | D] = Cards,
                {[[C1, C2, C3, C4], [C5, C6, C7, C8]], D};
            3 ->
                [C1, C2, C3, C4, C5, C6, C7, C8, C9 | D] = Cards,
                {[[C1, C2, C3], [C4, C5, C6], [C7, C8, C9]], D};
            4 ->
                [C1, C2, C3, C4, C5, C6, C7, C8 | D] = Cards,
                {[[C1, C2], [C3, C4], [C5, C6], [C7, C8]], D}
        end,
    SortedHands = array:from_list([array:from_list(lists:sort(C)) || C <- Hands]),
    #board{hands = SortedHands, deck = Deck}.

can_stay(Board) -> Board#board.can_stay.

num_players(Board) -> array:size(Board#board.hands).

attacker_card(Board) -> Board#board.attacker_card.

current_turn(Board) -> Board#board.turn.

next_turn(Board = #board{}) ->
    case check_finished(Board) of
        {finished, _} ->
            erlang:error(game_already_finished);
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
        [] -> none;
        [#card{num = N} | _] -> {N rem 2, true}
    end.

choose_attacker_card(Board = #board{deck = Deck}, HandIndex) ->
    case Deck of
        [C | NewDeck] ->
            % Deck is not empty.
            Board#board{deck = NewDeck, attacker_card = {deck, C}};
        [] ->
            % Deck is empty! Use HandIndex.
            % The chosen card should be hidden.
            C = get_hand(Board, current_turn(Board), HandIndex),
            case C#card.hidden of
                false -> erlang:error(invalid_card_state);
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
            erlang:error(attacker_card_not_set);
        {_, true} ->
            erlang:error(invalid_player_index);
        _ ->
            case get_hand(Board, TargetPlayer, TargetIndex) of
                #card{hidden = false} -> erlang:error(invalid_card_state);
                #card{hidden = true, num = Guess} -> success;
                _ -> failure
            end
    end.

reset_attacker_card(Board = #board{}, Hidden) ->
    Turn = current_turn(Board),
    NewBoard =
        case Board#board.attacker_card of
            {deck, C} ->
                OldHand = array:to_list(get_hand(Board, Turn)),
                NewHand = [C#card{hidden = Hidden} | OldHand],
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
    erlang:error(cannot_stay).

%%% private functions
shuffle_list(L) when is_list(L) ->
    % Thanks to: https://stackoverflow.com/a/8820501
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

get_hand(Board = #board{hands = Hands}, PlayerIndex) ->
    case 1 =< PlayerIndex andalso PlayerIndex =< num_players(Board) of
        false ->
            erlang:error(invalid_player_index);
        true ->
            array:get(PlayerIndex - 1, Hands)
    end.

get_hand(Board = #board{}, PlayerIndex, HandIndex) ->
    Hand = get_hand(Board, PlayerIndex),
    case 1 =< HandIndex andalso HandIndex =< array:size(Hand) of
        false -> erlang:error(invalid_hand_index);
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
            erlang:error(invalid_card_state);
        _ ->
            NewCard = OldCard#card{hidden = false},
            set_hand(Board, PlayerIndex, HandIndex, NewCard)
    end.
