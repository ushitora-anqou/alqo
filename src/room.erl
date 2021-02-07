-module(room).
-behaviour(gen_server).

-export([start_link/1, register_as_player/1, initial_state/1, attack/5, stay/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type state() :: {not_started, integer(), integer()} | {playing, any()}.

start_link(NumPlayers) ->
    gen_server:start_link(?MODULE, NumPlayers, []).

register_as_player(Pid) ->
    gen_server:call(Pid, register_as_player).

attack(Pid, PlayerIndex, TargetPlayer, TargetIndex, Guess) ->
    gen_server:call(Pid, {attack, PlayerIndex, TargetPlayer, TargetIndex, Guess}).

stay(Pid, PlayerIndex) ->
    gen_server:call(Pid, {stay, PlayerIndex}).

-spec initial_state(integer()) -> state().
initial_state(NumPlayers) ->
    {not_started, 0, NumPlayers}.

%%% GEN_SERVER CALLBACKS
init(RoomID) ->
    gen_server:cast(self(), {set_pid, RoomID}),
    {ok, RoomID}.

handle_call(Event, From, RoomID) ->
    State = room_database:get_current_state(RoomID),
    case state_has_finished(State) of
        true ->
            % Reply error if game has already finished
            {reply, {error, game_already_finished}, RoomID};
        false ->
            case handle_call_impl(Event, From, RoomID, State) of
                {reply, Reply, NewState} ->
                    room_database:set_current_state(RoomID, NewState),
                    {reply, Reply, RoomID};
                {noreply, NewState} ->
                    room_database:set_current_state(RoomID, NewState),
                    {noreply, RoomID}
            end
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
handle_call_impl(
    {attack, PlayerIndex, TargetPlayer, TargetIndex, Guess},
    _From,
    RoomID,
    {playing, Board}
) ->
    case PlayerIndex =:= game:current_turn(Board) of
        false ->
            {reply, {error, not_current_turn}, {playing, Board}};
        true ->
            try
                {Result, Board1} = game:attack(Board, TargetPlayer, TargetIndex, Guess),
                Board2 = choose_attacker_card_if_possible(Board1),

                % Send attacked event
                room_database:ws_send_to_all_in_room(
                    RoomID,
                    {attacked, Board2, TargetPlayer, TargetIndex, Guess, Result}
                ),

                % Send game_finished event if game has finished
                case game:check_finished(Board2) of
                    not_finished ->
                        ok;
                    {finished, Winner} ->
                        room_database:ws_send_to_all_in_room(RoomID, {game_finished, Winner})
                end,

                {reply, {ok, Result}, {playing, Board2}}
            catch
                throw:Reason ->
                    {reply, {error, Reason}, {playing, Board}}
            end
    end;
handle_call_impl({attack, _, _, _, _}, _From, _RoomID, State) ->
    {reply, {error, not_started}, State};
%
handle_call_impl({stay, PlayerIndex}, _From, RoomID, {playing, Board}) ->
    case {PlayerIndex =:= game:current_turn(Board), game:can_stay(Board)} of
        {false, _} ->
            {reply, {error, not_current_turn}, {playing, Board}};
        {_, false} ->
            {reply, {error, cannot_stay}, {playing, Board}};
        _ ->
            Board1 = game:stay(Board),
            Board2 = choose_attacker_card_if_possible(Board1),

            % Send stayed event
            room_database:ws_send_to_all_in_room(RoomID, {stayed, Board2}),

            {reply, ok, {playing, Board2}}
    end;
handle_call_impl({stay, _}, _From, _RoomID, State) ->
    {reply, {error, not_started}, State};
%
handle_call_impl(_Event, _From, _RoomID, State) ->
    {noreply, State}.

choose_attacker_card_if_possible(Board) ->
    case {game:attacker_card(Board), game:get_deck_top_from_others(Board)} of
        {undefined, undefined} ->
            % Need explicitly choosing
            Board;
        {undefined, _} ->
            game:choose_attacker_card(Board);
        _ ->
            Board
    end.

state_has_finished({playing, Board}) ->
    case game:check_finished(Board) of
        not_finished -> false;
        _ -> true
    end;
state_has_finished(_) ->
    false.
