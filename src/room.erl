-module(room).
-behaviour(gen_server).

-export([
    create_one/1,
    start_link/1,
    exists/1,
    register_as_player/1,
    attack/5,
    stay/2,
    state_to_json/2,
    get_num_players/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type state() :: {not_started, integer(), integer()} | {playing, any()}.

create_one(NumPlayers) ->
    InitialRoomState = {not_started, 0, NumPlayers},
    RoomID = room_database:create_room(InitialRoomState),
    % Room should be created AFTER room state is set
    room_sup:create_room(RoomID),
    RoomID.

start_link(NumPlayers) ->
    gen_server:start_link(?MODULE, NumPlayers, []).

exists(RoomID) ->
    get_pid(RoomID) =/= undefined.

register_as_player(RoomID) ->
    case get_pid(RoomID) of
        undefined -> throw(room_not_found);
        Pid -> gen_server:call(Pid, register_as_player)
    end.

attack(RoomID, PlayerIndex, TargetPlayer, TargetIndex, Guess) ->
    case get_pid(RoomID) of
        undefined -> throw(room_not_found);
        Pid -> gen_server:call(Pid, {attack, PlayerIndex, TargetPlayer, TargetIndex, Guess})
    end.

stay(RoomID, PlayerIndex) ->
    case get_pid(RoomID) of
        undefined -> throw(room_not_found);
        Pid -> gen_server:call(Pid, {stay, PlayerIndex})
    end.

state_to_json(RoomID, PlayerIndex) ->
    case room_database:get_current_state(RoomID) of
        undefined -> throw(room_not_found);
        RoomState -> state_to_json_impl(RoomState, PlayerIndex)
    end.

get_num_players(RoomID) ->
    case room_database:get_current_state(RoomID) of
        undefined -> throw(room_not_found);
        {not_started, _, NumPlayers} -> NumPlayers;
        {playing, Board} -> game:num_players(Board)
    end.

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
    set_pid(RoomID),
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

get_pid(RoomID) ->
    gproc:where(gproc_room_id(RoomID)).

set_pid(RoomID) ->
    gproc:reg(gproc_room_id(RoomID)).

gproc_room_id(RoomID) ->
    {n, l, {alqo_room, RoomID}}.

state_to_json_impl({not_started, RegisteredNumPlayers, NumPlayers}, PlayerIndex) ->
    jsone:encode(#{
        status => <<"not_started">>,
        registered => RegisteredNumPlayers,
        nplayers => NumPlayers,
        your_status =>
            case PlayerIndex of
                undefined -> false;
                _ -> true
            end
    });
state_to_json_impl({playing, Board}, PlayerIndex) ->
    jsone:encode(#{
        status => <<"playing">>,
        board => game:board_to_map(Board, PlayerIndex)
    }).

handle_call_impl(
    register_as_player,
    _From,
    RoomID,
    {not_started, RegisteredNumPlayers, NumPlayers}
) when RegisteredNumPlayers < NumPlayers ->
    NewPlayerIndex = RegisteredNumPlayers + 1,
    ws_room:send_to_all_in_room(RoomID, {player_registered, NewPlayerIndex}),
    case RegisteredNumPlayers + 1 =:= NumPlayers of
        true ->
            Board = game:choose_attacker_card(game:new_board(NumPlayers)),

            % Send event game_started
            ws_room:send_to_all_in_room(RoomID, {game_started, Board}),
            % Send each hand
            lists:foreach(
                fun(PI) ->
                    HandNums = [N || {N, _H} <- game:hand(Board, PI)],
                    ws_room:send(RoomID, PI, {your_hand, HandNums})
                end,
                lists:seq(1, NumPlayers)
            ),
            % Send event your_turn to player 1
            ws_send_your_turn(RoomID, Board),

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
                ws_room:send_to_all_in_room(
                    RoomID,
                    {attacked, Board2, TargetPlayer, TargetIndex, Guess, Result}
                ),

                % Send game_finished event if game has finished
                case game:check_finished(Board2) of
                    not_finished ->
                        ok;
                    {finished, Winner} ->
                        ws_room:send_to_all_in_room(RoomID, {game_finished, Winner})
                end,

                % Send your_turn event if turn changed
                case game:current_turn(Board2) =:= game:next_turn(Board) of
                    false -> ok;
                    true -> ws_send_your_turn(RoomID, Board2)
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
            ws_room:send_to_all_in_room(RoomID, {stayed, Board2}),
            % Send your_turn event
            ws_send_your_turn(RoomID, Board2),

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

ws_send_your_turn(RoomID, Board) ->
    ws_room:send(
        RoomID,
        game:current_turn(Board),
        {your_turn,
            case game:attacker_card(Board) of
                undefined -> null;
                {deck, N} -> N
            end}
    ).
