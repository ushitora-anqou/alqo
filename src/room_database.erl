-module(room_database).
-include_lib("stdlib/include/ms_transform.hrl").
-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    create_room/1,
    get_pid/1,
    set_pid/1,
    get_current_state/1,
    set_current_state/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(room, {
    roomid :: binary(),
    pid :: pid() | undefined,
    ref :: reference() | undefined,
    state :: any()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

create_room(NumPlayers) ->
    InitialRoomState = room:initial_state(NumPlayers),
    gen_server:call(?MODULE, {create, InitialRoomState}).

% may return undefined
get_pid(RoomID) ->
    case ets:lookup(?MODULE, RoomID) of
        [] -> undefined;
        [#room{pid = Pid}] -> Pid
    end.

set_pid(RoomID) ->
    gen_server:call(?MODULE, {set_pid, RoomID, self()}).

get_current_state(RoomID) ->
    case ets:lookup(?MODULE, RoomID) of
        [] -> undefined;
        [#room{state = State}] -> State
    end.

set_current_state(RoomID, NewState) ->
    gen_server:call(?MODULE, {set_current_state, RoomID, self(), NewState}).

%%% GEN_SERVER CALLBACKS
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected, {keypos, #room.roomid}]),
    {ok, ?MODULE}.

handle_call({create, RoomState}, _From, State) ->
    RoomID = list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)),
    room_sup:create_room(RoomID),
    % Event set_pid will come only after event create is done.
    % XXX assume RoomID does not duplicate
    true = ets:insert_new(
        State,
        #room{roomid = RoomID, pid = undefined, ref = undefined, state = RoomState}
    ),
    {reply, RoomID, State};
handle_call({set_pid, RoomID, RoomPid}, _From, State) ->
    case ets:lookup(State, RoomID) of
        [] ->
            throw(room_not_found);
        [#room{pid = RoomPid}] ->
            % The same process; ok.
            {reply, ok, State};
        [Room = #room{pid = undefined, ref = undefined}] ->
            % not associated with room process yet; ok.
            RoomRef = monitor(process, RoomPid),
            ets:insert(State, Room#room{pid = RoomPid, ref = RoomRef}),
            {reply, ok, State};
        _ ->
            error(room_process_duplicate)
    end;
handle_call({set_current_state, RoomID, RoomPid, RoomNewState}, _From, State) ->
    case ets:lookup(State, RoomID) of
        [] ->
            throw(room_not_found);
        [Room = #room{pid = RoomPid}] ->
            % Correct process; ok.
            ets:insert(State, Room#room{state = RoomNewState}),
            {reply, ok, State};
        _ ->
            error(invalid_process_setting_state)
    end;
handle_call(stop, _From, State) ->
    ets:delete(State),
    {stop, normal, ok, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', RoomRef, process, RoomPid, _Reason}, State) ->
    MatchSpec = ets:fun2ms(fun(Room = #room{}) when
        RoomPid =:= Room#room.pid, RoomRef =:= Room#room.ref
    ->
        Room
    end),
    case ets:select(State, MatchSpec) of
        [] ->
            throw(room_not_found);
        [Room = #room{}] ->
            ets:insert(State, Room#room{pid = undefined, ref = undefined})
    end,
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
