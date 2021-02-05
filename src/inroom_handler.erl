-module(inroom_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    resource_exists/2,
    current_room_json/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, current_room_json}], Req, State}.

resource_exists(Req, RoomID) ->
    Exists =
        case room_database:get_pid(RoomID) of
            undefined -> false;
            _Pid -> true
        end,
    {Exists, Req, RoomID}.

current_room_json(Req, RoomID) ->
    RoomState = room_database:get_current_state(RoomID),
    {PlayerIndex, Req1} = cowboy_session:get({player_index, RoomID}, Req),
    Body = room_state_to_json(RoomState, PlayerIndex),
    {Body, Req1, RoomID}.

%% Private functions
room_state_to_json({not_started, RegisteredNumPlayers, NumPlayers}, PlayerIndex) ->
    jsone:encode(#{
        status => <<"not_started">>,
        registered => RegisteredNumPlayers,
        nplayers => NumPlayers,
        your_status =>
            case PlayerIndex of
                undefined -> false;
                _ -> true
            end
    });
room_state_to_json({playing, Board}, PlayerIndex) ->
    jsone:encode(#{
        status => <<"playing">>,
        board => board_to_map(Board, PlayerIndex)
    }).

board_to_map(Board, PlayerIndex) ->
    Map = #{
        can_stay => game:can_stay(Board),
        current_turn => game:current_turn(Board),
        next_turn => game:next_turn(Board),
        num_players => game:num_players(Board),
        winner =>
            case game:check_finished(Board) of
                not_finished -> null;
                {finished, Winner} -> Winner
            end,
        hands => [
            [[N, H] || {N, H} <- game:hand_from_others(Board, PI)]
            || PI <- lists:seq(1, game:num_players(Board))
        ],
        deck_top => game:get_deck_top_from_others(Board),
        attacker_card =>
            case game:attacker_card(Board) of
                undefined -> null;
                {deck, {N, _H}} -> [1, N];
                {hand, HI} -> [2, HI]
            end
    },
    case PlayerIndex of
        undefined ->
            Map;
        PI ->
            Map#{
                your_player_index => PI,
                your_hand => [N || {N, _H} <- game:hand(Board, PI)],
                your_attacker_card_from_deck =>
                    case {game:current_turn(Board), game:attacker_card(Board)} of
                        {Turn, {deck, C}} when Turn =:= PI -> card_tuple_to_list(C);
                        _ -> null
                    end
            }
    end.

card_tuple_to_list({N, H}) -> [N, H];
card_tuple_to_list(none) -> null.
