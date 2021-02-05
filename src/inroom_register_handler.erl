-module(inroom_register_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    register_user/2,
    resource_exists/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    % For 201 Created
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, register_user}], Req, State}.

register_user(Req, RoomID) ->
    case {cowboy_session:get({player_index, RoomID}, Req), room_database:get_pid(RoomID)} of
        {{_, Req1}, undefined} ->
            {false, Req1, RoomID};
        {{undefined, Req1}, Pid} ->
            case room:register_as_player(Pid) of
                {ok, PlayerIndex} ->
                    {ok, Req2} = cowboy_session:set({player_index, RoomID}, PlayerIndex, Req1),
                    {{true, <<<<"/room/">>/binary, RoomID/binary>>}, Req2, RoomID};
                {error, _} ->
                    {false, Req1, RoomID}
            end;
        {{_, Req1}, _} ->
            {false, Req1, RoomID}
    end.
