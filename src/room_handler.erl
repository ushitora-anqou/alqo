-module(room_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    create_room/2,
    resource_exists/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    % For 201 Created
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, create_room}], Req, State}.

create_room(Req0, State) ->
    {ok, ReqData, Req} = cowboy_req:read_body(Req0),
    case jsone:decode(ReqData) of
        #{<<"nplayers">> := NumPlayers} when
            is_integer(NumPlayers), 2 =< NumPlayers, NumPlayers =< 4
        ->
            RoomID = room:create_one(NumPlayers),
            {{true, <<<<"/room/">>/binary, RoomID/binary>>}, Req, State};
        _ ->
            {false, Req, State}
    end.

%% Private funtions
