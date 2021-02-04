-module(inroom_register_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    register_user/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, register_user}], Req, State}.

register_user(Req, RoomID) ->
    case room_database:get_pid(RoomID) of
        undefined ->
            {false, Req, RoomID};
        Pid ->
            case room:register_as_player(Pid) of
                {ok, _} ->
                    {{true, <<<<"/room/">>/binary, RoomID/binary>>}, Req, RoomID};
                {error, _} ->
                    {false, Req, RoomID}
            end
    end.
