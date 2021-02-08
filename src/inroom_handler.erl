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
    {room:exists(RoomID), Req, RoomID}.

current_room_json(Req, RoomID) ->
    {PlayerIndex, Req1} = cowboy_session:get({player_index, RoomID}, Req),
    Body = room:state_to_json(RoomID, PlayerIndex),
    {Body, Req1, RoomID}.

%% Private functions
