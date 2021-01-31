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
    RoomID = new_room_id(),
    {ok, [{<<"nplayers">>, NumPlayersStr}], Req2} = cowboy_req:read_urlencoded_body(Req),
    NumPlayers = binary_to_integer(NumPlayersStr),
    case 2 =< NumPlayers andalso NumPlayers =< 4 of
        false ->
            {false, Req2, State};
        true ->
            Board = game:new_board(NumPlayers),
            board_database:register_board(RoomID, Board),
            {{true, <<$/, RoomID/binary>>}, Req2, State}
    end.

%% Private funtions

new_room_id() ->
    Initial = rand:uniform(62) - 1,
    new_room_id(<<Initial>>, 7).
new_room_id(Bin, 0) ->
    Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
    <<<<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin>>;
new_room_id(Bin, Rem) ->
    Next = rand:uniform(62) - 1,
    new_room_id(<<Bin/binary, Next>>, Rem - 1).
