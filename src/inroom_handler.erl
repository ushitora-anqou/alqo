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
        board => game:board_to_map(Board, PlayerIndex)
    }).
