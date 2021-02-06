-module(room_database).
-behaviour(gen_server).
-export([
    start_link/0,
    stop/0,
    create_room/1,
    get_pid/1,
    set_pid/1,
    set_ws_pid/2,
    ws_send/3,
    ws_send_to_all_in_room/2,
    get_current_state/1,
    set_current_state/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

create_room(NumPlayers) ->
    InitialRoomState = room:initial_state(NumPlayers),
    gen_server:call(?MODULE, {create, InitialRoomState}).

get_pid(RoomID) ->
    gproc:where(gproc_room_id(RoomID)).

set_pid(RoomID) ->
    gproc:reg(gproc_room_id(RoomID)).

set_ws_pid(RoomID, PlayerIndex) ->
    gproc:reg(gproc_ws_room_id(RoomID, PlayerIndex)).

ws_send(RoomID, PlayerIndex, Msg) ->
    gproc:send(gproc_ws_room_id(RoomID, PlayerIndex), Msg).

ws_send_to_all_in_room(RoomID, Msg) ->
    NumPlayers = get_num_players(RoomID),
    lists:foreach(
        fun(PI) -> ws_send(RoomID, PI, Msg) end,
        [undefined | lists:seq(1, NumPlayers)]
    ).

get_current_state(RoomID) ->
    case ets:lookup(?MODULE, RoomID) of
        [] -> undefined;
        [{_, State}] -> State
    end.

set_current_state(RoomID, RoomNewState) ->
    gen_server:call(?MODULE, {set_current_state, RoomID, RoomNewState}).

get_num_players(RoomID) ->
    case get_current_state(RoomID) of
        undefined -> throw(room_not_found);
        {not_started, _, NumPlayers} -> NumPlayers;
        {playing, Board} -> game:num_players(Board)
    end.

%% GEN_SERVER CALLBACKS
init(_) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.

handle_call({create, RoomState}, _From, State) ->
    RoomID = list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)),
    % XXX assume RoomID does not duplicate
    true = ets:insert_new(?MODULE, {RoomID, RoomState}),
    % Room should be created AFTER room state is set
    room_sup:create_room(RoomID),
    {reply, RoomID, State};
handle_call({set_current_state, RoomID, RoomNewState}, _From, State) ->
    ets:insert(?MODULE, {RoomID, RoomNewState}),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    ets:delete(?MODULE),
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

gproc_room_id(RoomID) ->
    {n, l, {alqo_room, RoomID}}.

gproc_ws_room_id(RoomID, PlayerIndex) ->
    {p, l, {alqo_ws_room, RoomID, PlayerIndex}}.
