-module(ws_room).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {PlayerIndex, Req1} = cowboy_session:get({player_index, RoomID}, Req),
    {cowboy_websocket, Req1, {PlayerIndex, RoomID}}.

websocket_init(State) ->
    {[], State}.

%websocket_handle({text, Msg}, State) ->
%    {[{text, <<"Hello", Msg/binary>>}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info(_Info, State) ->
    {[], State}.
