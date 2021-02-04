-module(room_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    create_room/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, create_room}], Req, State}.

create_room(Req, State) ->
    {ok, [{<<"nplayers">>, NumPlayersStr}], Req2} = cowboy_req:read_urlencoded_body(Req),
    NumPlayers = binary_to_integer(NumPlayersStr),
    case 2 =< NumPlayers andalso NumPlayers =< 4 of
        false ->
            {false, Req2, State};
        true ->
            RoomID = room_database:create_room(NumPlayers),
            {{true, <<<<"/room/">>/binary, RoomID/binary>>}, Req2, State}
    end.

%% Private funtions
