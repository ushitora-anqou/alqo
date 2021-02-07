-module(ws_room).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {PlayerIndex, Req1} = cowboy_session:get({player_index, RoomID}, Req),
    {cowboy_websocket, Req1, {RoomID, PlayerIndex}}.

websocket_init(State = {RoomID, PlayerIndex}) ->
    room_database:set_ws_pid(RoomID, PlayerIndex),
    {[], State}.

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({player_registered, PlayerIndex}, State) ->
    {[{text, jsone:encode([<<"player_registered">>, PlayerIndex])}], State};
websocket_info({game_started, Board}, State) ->
    {[{text, jsone:encode([<<"game_started">>, game:board_to_map(Board)])}], State};
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
websocket_info(_Info, State) ->
    {[], State}.
