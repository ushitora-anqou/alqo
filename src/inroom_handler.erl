-module(inroom_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    resource_exists/2,
    current_board_json/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, current_board_json}], Req, State}.

resource_exists(Req, RoomID) ->
    {undefined =/= board_database:get_board_by_name(RoomID), Req, RoomID}.

current_board_json(Req, RoomID) ->
    case board_database:get_board_by_name(RoomID) of
        undefined ->
            erlang:error(room_not_found);
        Board ->
            Body = jsone:encode(#{
                can_stay => game:can_stay(Board),
                current_turn => game:current_turn(Board),
                next_turn => game:next_turn(Board),
                num_players => game:num_players(Board),
                winner =>
                    case game:check_finished(Board) of
                        not_finished -> null;
                        {finished, Winner} -> Winner
                    end,
                your_player_index => null,
                hands => [
                    [card_tuple_to_list(T) || T <- game:hand_from_others(Board, PI)]
                    || PI <- lists:seq(1, game:num_players(Board))
                ],
                deck_top => card_tuple_to_list(game:get_deck_top_from_others(Board)),
                attacker_card => null
                %case game:attacker_card(Board) of
                %    undefined -> null;
                %    {deck, C} -> #{kind => 1, card => card_tuple_to_list(C)};
                %    {hand, HI} -> #{kind => 2, index => HI}
                %end
            }),
            {Body, Req, RoomID}
    end.

%% Private functions

card_tuple_to_list({N, H}) -> [N, H];
card_tuple_to_list(none) -> null.
