-type card_number() :: card_number().
-type player_index() :: pos_integer().
-type hand_index() :: pos_integer().

-record(card, {
    % The number written on the card, which is even if black, and odd if white.
    num :: card_number(),
    hidden :: boolean(),
    uuid :: uuid:uuid()
}).
-type card() :: #card{}.

-record(board, {
    hands :: array:array(array:array(card)),
    deck :: [card()],
    turn = 1 :: player_index(),
    can_stay = false :: boolean(),
    attacker_card :: 'undefined' | {'deck', card()} | {'hand', hand_index()}
}).
-type board() :: #board{}.
