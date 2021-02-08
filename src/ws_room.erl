-module(ws_room).
-behaviour(cowboy_handler).
-export([send/3, send_to_all_in_room/2]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

send(RoomID, PlayerIndex, Msg) ->
    gproc:send(gproc_ws_room_id(RoomID, PlayerIndex), Msg).

send_to_all_in_room(RoomID, Msg) ->
    NumPlayers = room:get_num_players(RoomID),
    lists:foreach(
        fun(PI) -> send(RoomID, PI, Msg) end,
        [undefined | lists:seq(1, NumPlayers)]
    ).

%% COWBOY HANDLER

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {PlayerIndex, Req1} = cowboy_session:get({player_index, RoomID}, Req),
    {cowboy_websocket, Req1, {RoomID, PlayerIndex}}.

websocket_init(State = {RoomID, PlayerIndex}) ->
    set_ws_pid(RoomID, PlayerIndex),
    {[], State}.

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({player_registered, PlayerIndex}, State) ->
    {[{text, jsone:encode([<<"player_registered">>, PlayerIndex])}], State};
websocket_info({game_started, Board}, State) ->
    {[{text, jsone:encode([<<"game_started">>, game:board_to_map(Board)])}], State};
websocket_info({your_hand, HandNums}, State) ->
    {[{text, jsone:encode([your_hand, HandNums])}], State};
websocket_info({your_turn, AttackerCardNum}, State) ->
    {[{text, jsone:encode([your_turn, AttackerCardNum])}], State};
websocket_info({attacked, NewBoard, TargetPlayer, TargetHandIndex, Guess, Result}, State) ->
    {[
            {text,
                jsone:encode([
                    attacked,
                    #{
                        board => game:board_to_map(NewBoard),
                        target_player => TargetPlayer,
                        target_hand_index => TargetHandIndex,
                        guess => Guess,
                        result =>
                            case Result of
                                success -> true;
                                failure -> false
                            end
                    }
                ])}
        ],
        State};
websocket_info({stayed, NewBoard}, State) ->
    {[{text, jsone:encode([stayed, game:board_to_map(NewBoard)])}], State};
websocket_info({game_finished, Winner}, State) ->
    {[{text, jsone:encode([game_finished, Winner])}], State};
websocket_info(_Info, State) ->
    {[], State}.

%% internal functions

set_ws_pid(RoomID, PlayerIndex) ->
    gproc:reg(gproc_ws_room_id(RoomID, PlayerIndex)).

gproc_ws_room_id(RoomID, PlayerIndex) ->
    {p, l, {alqo_ws_room, RoomID, PlayerIndex}}.
