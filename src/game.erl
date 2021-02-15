-module(game).
-include("game.hrl").
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
    deck_top/1,
    choose_attacker_card/1,
    choose_attacker_card/2,
    attack/4,
    stay/1,
    board_to_map/2,
    board_to_map/1
    %    attacker_card_from_others/1,
    %    hand_from_others/2,
    %    get_deck_top_from_others/1,
]).

-spec new_board(2..4) -> board().
new_board(NumPlayers) ->
    Cards = [
        #card{num = N, hidden = true, uuid = gen_uuid()}
        || N <- shuffle_list(lists:seq(0, 23))
    ],
    {HandList, Deck} =
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
    SortedHandList = [sorted_cards(L) || L <- HandList],
    Hands = nested_list_to_array(SortedHandList),
    #board{hands = Hands, deck = Deck, turn = 1, can_stay = false, attacker_card = undefined}.

-spec can_stay(board()) -> boolean().
can_stay(Board) -> Board#board.can_stay.

-spec num_players(board()) -> pos_integer().
num_players(Board) -> array:size(Board#board.hands).

-spec attacker_card(board()) -> 'undefined' | {'deck', card()} | {'hand', hand_index()}.
attacker_card(Board) -> Board#board.attacker_card.

-spec current_turn(board()) -> player_index().
current_turn(Board) -> Board#board.turn.

-spec next_turn(board()) -> player_index().
next_turn(Board) ->
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

-spec has_player_lost(board(), player_index()) -> boolean().
has_player_lost(Board, PlayerIndex) ->
    % i.e., all cards of hand are revealed?
    lists:all(fun(C) -> not C#card.hidden end, hand(Board, PlayerIndex)).

-spec check_finished(board()) -> not_finished | {finished, player_index()}.
check_finished(Board) ->
    L = [has_player_lost(Board, PI) || PI <- lists:seq(1, num_players(Board))],
    case count_true(L) >= num_players(Board) - 1 of
        false -> not_finished;
        true -> {finished, util:find_first_index(fun(B) -> not B end, L)}
    end.

-spec hand(board(), player_index()) -> [card()].
hand(Board, PlayerIndex) ->
    array:to_list(get_hand(Board, PlayerIndex)).

-spec deck_top(board()) -> 'undefined' | card().
deck_top(#board{deck = []}) -> undefined;
deck_top(#board{deck = [C | _]}) -> C.

-spec choose_attacker_card(board(), 'undefined' | hand_index()) -> board().
choose_attacker_card(Board = #board{deck = [C | NewDeck]}, _) ->
    % Deck is not empty.
    Board#board{deck = NewDeck, attacker_card = {deck, C}};
choose_attacker_card(Board = #board{deck = []}, HandIndex) ->
    % Deck is empty! Use HandIndex.
    % The chosen card should be hidden.
    C = get_hand(Board, current_turn(Board), HandIndex),
    case C#card.hidden of
        false -> throw(invalid_card_state);
        true -> Board#board{attacker_card = {hand, HandIndex}}
    end.

-spec choose_attacker_card(board()) -> board().
choose_attacker_card(Board) ->
    choose_attacker_card(Board, undefined).

-spec attack(board(), player_index(), hand_index(), card_number()) ->
    {'failure', board()} | {'success', board()}.
attack(Board, TargetPlayer, TargetIndex, Guess) ->
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

-spec stay(board()) -> board().
stay(Board = #board{can_stay = true}) ->
    NewBoard = reset_attacker_card(Board, true),
    NewBoard#board{
        turn = next_turn(Board),
        can_stay = false
    };
stay(#board{can_stay = false}) ->
    throw(cannot_stay).

-spec board_to_map(board(), 'undefined' | player_index()) -> #{any() => any()}.
board_to_map(Board, PlayerIndex) ->
    Card2List = fun
        (#card{num = N, hidden = true, uuid = UUID}) -> [N rem 2, true, UUID];
        (#card{num = N, hidden = false, uuid = UUID}) -> [N, false, UUID];
        (undefined) -> null
    end,

    Map = #{
        can_stay => can_stay(Board),
        current_turn => current_turn(Board),
        next_turn =>
            case check_finished(Board) of
                not_finished -> next_turn(Board);
                _ -> null
            end,
        num_players => num_players(Board),
        winner =>
            case check_finished(Board) of
                not_finished -> null;
                {finished, Winner} -> Winner
            end,
        hands => [[Card2List(C) || C <- hand(Board, PI)] || PI <- lists:seq(1, num_players(Board))],
        deck_top => Card2List(deck_top(Board)),
        attacker_card =>
            case attacker_card(Board) of
                undefined -> null;
                {deck, C} -> [1, Card2List(C)];
                {hand, HI} -> [2, HI]
            end
    },
    case PlayerIndex of
        undefined ->
            Map;
        PI ->
            Map#{
                your_player_index => PI,
                your_hand => [C#card.num || C <- hand(Board, PI)],
                your_attacker_card_from_deck =>
                    case {current_turn(Board), attacker_card(Board)} of
                        {PI, {deck, #card{num = N1}}} -> N1;
                        _ -> null
                    end
            }
    end.

board_to_map(Board) ->
    board_to_map(Board, undefined).

%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%
shuffle_list(L) when is_list(L) ->
    % Thanks to: https://stackoverflow.com/a/8820501
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

nested_list_to_array(L) when is_list(L) -> array:from_list([nested_list_to_array(E) || E <- L]);
nested_list_to_array(E) -> E.

gen_uuid() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)).

count_true([], Sum) -> Sum;
count_true([H | T], Sum) when H =:= true -> count_true(T, Sum + 1);
count_true([H | T], Sum) when H =:= false -> count_true(T, Sum).

count_true(L) -> count_true(L, 0).

-spec get_hand(board(), player_index()) -> array:array(card()).
get_hand(Board = #board{hands = Hands}, PlayerIndex) ->
    case 1 =< PlayerIndex andalso PlayerIndex =< num_players(Board) of
        false ->
            throw(invalid_player_index);
        true ->
            array:get(PlayerIndex - 1, Hands)
    end.

-spec get_hand(board(), player_index(), hand_index()) -> card().
get_hand(Board = #board{}, PlayerIndex, HandIndex) ->
    Hand = get_hand(Board, PlayerIndex),
    case 1 =< HandIndex andalso HandIndex =< array:size(Hand) of
        false -> throw(invalid_hand_index);
        true -> array:get(HandIndex - 1, Hand)
    end.

-spec set_hand(board(), player_index(), array:array(card())) -> board().
set_hand(Board = #board{hands = OldHands}, PlayerIndex, NewHand) ->
    NewHands = array:set(PlayerIndex - 1, NewHand, OldHands),
    Board#board{hands = NewHands}.

-spec set_hand(board(), player_index(), hand_index(), card()) -> board().
set_hand(Board, PlayerIndex, HandIndex, NewCard) ->
    OldHand = get_hand(Board, PlayerIndex),
    NewHand = array:set(HandIndex - 1, NewCard, OldHand),
    set_hand(Board, PlayerIndex, NewHand).

-spec reveal_hand(board(), player_index(), hand_index()) -> board().
reveal_hand(Board, PlayerIndex, HandIndex) ->
    OldCard = get_hand(Board, PlayerIndex, HandIndex),
    case OldCard of
        #card{hidden = false} ->
            throw(invalid_card_state);
        _ ->
            NewCard = OldCard#card{hidden = false},
            set_hand(Board, PlayerIndex, HandIndex, NewCard)
    end.

-spec check_attack(board(), player_index(), hand_index(), card_number()) -> 'success' | 'failure'.
check_attack(#board{attacker_card = undefined}, _, _, _) ->
    throw(attacker_card_not_set);
check_attack(Board, TargetPlayer, _, _) when TargetPlayer =:= Board#board.turn ->
    throw(invalid_player_index);
check_attack(Board, TargetPlayer, TargetIndex, Guess) ->
    case get_hand(Board, TargetPlayer, TargetIndex) of
        #card{hidden = false} -> throw(invalid_card_state);
        #card{hidden = true, num = Guess} -> success;
        _ -> failure
    end.

-spec sorted_cards([card()]) -> [card()].
sorted_cards(L) ->
    lists:sort(fun(#card{num = A}, #card{num = B}) -> A =< B end, L).

-spec reset_attacker_card(board(), boolean()) -> board().
reset_attacker_card(Board, Hidden) ->
    Turn = current_turn(Board),
    NewBoard =
        case Board#board.attacker_card of
            {deck, C} ->
                OldHand = array:to_list(get_hand(Board, Turn)),
                NewHand = sorted_cards([C#card{hidden = Hidden} | OldHand]),
                set_hand(Board, Turn, array:from_list(NewHand));
            {hand, HI} when Hidden =:= false ->
                reveal_hand(Board, Turn, HI);
            {hand, _} ->
                Board
        end,
    NewBoard#board{
        attacker_card = undefined
    }.
