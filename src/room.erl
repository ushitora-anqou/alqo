-module(room).
-behaviour(gen_server).

-export([start_link/1, register_as_player/1, get_board/1, initial_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type state() :: {not_started, integer(), integer()} | {playing, any()}.

start_link(NumPlayers) ->
    gen_server:start_link(?MODULE, NumPlayers, []).

register_as_player(Pid) ->
    gen_server:call(Pid, register_as_player).

get_board(Pid) ->
    gen_server:call(Pid, get_board).

-spec initial_state(integer()) -> state().
initial_state(NumPlayers) ->
    {not_started, 0, NumPlayers}.

%%% GEN_SERVER CALLBACKS
init(RoomID) ->
    gen_server:cast(self(), {set_pid, RoomID}),
    {ok, RoomID}.

handle_call(Event, From, RoomID) ->
    State = room_database:get_current_state(RoomID),
    case handle_call_impl(Event, From, State) of
        {reply, Reply, NewState} ->
            room_database:set_current_state(RoomID, NewState),
            {reply, Reply, RoomID};
        {noreply, NewState} ->
            room_database:set_current_state(RoomID, NewState),
            {noreply, RoomID}
    end.

handle_cast({set_pid, RoomID}, State) ->
    room_database:set_pid(RoomID),
    {noreply, State};
handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _RoomID) ->
    ok.

%%% Private functions

handle_call_impl(register_as_player, _From, {not_started, RegisteredNumPlayers, NumPlayers}) when
    RegisteredNumPlayers < NumPlayers
->
    case RegisteredNumPlayers + 1 =:= NumPlayers of
        true ->
            {reply, {ok, playing}, {playing, game:new_board(NumPlayers)}};
        false ->
            {reply, {ok, not_started}, {not_started, RegisteredNumPlayers + 1, NumPlayers}}
    end;
handle_call_impl(register_as_player, _From, State) ->
    {reply, {error, playing}, State};
handle_call_impl(get_board, _From, {playing, Board}) ->
    {reply, {ok, Board}, {playing, Board}};
handle_call_impl(get_board, _From, State) ->
    {reply, {error, not_started}, State};
handle_call_impl(_Event, _From, State) ->
    {noreply, State}.
