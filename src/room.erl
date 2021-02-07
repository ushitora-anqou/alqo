-module(room).
-behaviour(gen_server).

-export([start_link/1, register_as_player/1, get_board/1, initial_state/1, attack/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type state() :: {not_started, integer(), integer()} | {playing, any()}.

start_link(NumPlayers) ->
    gen_server:start_link(?MODULE, NumPlayers, []).

register_as_player(Pid) ->
    gen_server:call(Pid, register_as_player).

get_board(Pid) ->
    gen_server:call(Pid, get_board).

attack(Pid, PlayerIndex, TargetPlayer, TargetIndex, Guess) ->
    gen_server:call(Pid, {attack, PlayerIndex, TargetPlayer, TargetIndex, Guess}).

-spec initial_state(integer()) -> state().
initial_state(NumPlayers) ->
    {not_started, 0, NumPlayers}.

%%% GEN_SERVER CALLBACKS
init(RoomID) ->
    gen_server:cast(self(), {set_pid, RoomID}),
    {ok, RoomID}.

handle_call(Event, From, RoomID) ->
    State = room_database:get_current_state(RoomID),
    case handle_call_impl(Event, From, RoomID, State) of
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

handle_call_impl(
    register_as_player,
    _From,
    RoomID,
    {not_started, RegisteredNumPlayers, NumPlayers}
) when RegisteredNumPlayers < NumPlayers ->
    NewPlayerIndex = RegisteredNumPlayers + 1,
    room_database:ws_send_to_all_in_room(RoomID, {player_registered, NewPlayerIndex}),
    case RegisteredNumPlayers + 1 =:= NumPlayers of
        true ->
            Board = game:choose_attacker_card(game:new_board(NumPlayers)),
            room_database:ws_send_to_all_in_room(RoomID, {game_started, Board}),
            {reply, {ok, NewPlayerIndex}, {playing, Board}};
        false ->
            {reply, {ok, NewPlayerIndex}, {not_started, RegisteredNumPlayers + 1, NumPlayers}}
    end;
handle_call_impl(register_as_player, _From, _RoomID, State) ->
    {reply, {error, playing}, State};
%
handle_call_impl(get_board, _From, _RoomID, {playing, Board}) ->
    {reply, {ok, Board}, {playing, Board}};
handle_call_impl(get_board, _From, _RoomID, State) ->
    {reply, {error, not_started}, State};
%
handle_call_impl(
    {attack, PlayerIndex, TargetPlayer, TargetIndex, Guess},
    _From,
    _RoomID,
    {playing, Board}
) ->
    case PlayerIndex =:= game:current_turn(Board) of
        false ->
            {reply, {error, not_current_turn}, {playing, Board}};
        true ->
            try
                {Result, Board1} = game:attack(Board, TargetPlayer, TargetIndex, Guess),
                Board2 =
                    case {game:attacker_card(Board1), game:get_deck_top_from_others(Board1)} of
                        {undefined, undefined} ->
                            % Need explicitly choosing
                            Board1;
                        {undefined, _} ->
                            game:choose_attacker_card(Board1);
                        _ ->
                            Board1
                    end,
                {reply, {ok, Result}, {playing, Board2}}
            catch
                throw:Reason ->
                    {reply, {error, Reason}, {playing, Board}}
            end
    end;
handle_call_impl(attack, _From, _RoomID, State) ->
    {reply, {error, not_started}, State};
%
handle_call_impl(_Event, _From, _RoomID, State) ->
    {noreply, State}.
