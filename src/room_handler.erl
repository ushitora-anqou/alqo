-module(room_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    forbidden/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2,
    allow_missing_post/2,
    provide_json/2,
    accept_json/2
]).

-record(state, {route :: {binary(), binary(), binary()}, player_index :: integer()}).

init(Req, Action) ->
    {RoomID, {PlayerIndex, Req1}} =
        case cowboy_req:binding(roomid, Req, undefined) of
            undefined ->
                {undefined, {undefined, Req}};
            RoomID1 ->
                {RoomID1, cowboy_session:get({player_index, RoomID1}, Req)}
        end,
    Method = cowboy_req:method(Req1),
    {cowboy_rest, Req1, #state{
        route = {Method, RoomID, Action},
        player_index = PlayerIndex
    }}.

forbidden(Req, State = #state{route = {_, undefined, _}}) ->
    {false, Req, State};
forbidden(Req, State = #state{route = {_, _RoomID, undefined}}) ->
    {false, Req, State};
forbidden(Req, State = #state{route = {_, _RoomID, register}}) ->
    {false, Req, State};
forbidden(Req, State = #state{route = {_, _RoomID, _Action}, player_index = undefined}) ->
    {true, Req, State};
forbidden(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State = #state{route = {_, undefined, undefined}}) ->
    % /room
    {[<<"POST">>], Req, State};
allowed_methods(Req, State = #state{route = {_, _RoomID, undefined}}) ->
    % /room/:roomid
    {[<<"GET">>], Req, State};
allowed_methods(Req, State = #state{route = {_, _RoomID, _Action}}) ->
    % /room/:roomid/:action
    {[<<"POST">>], Req, State}.

resource_exists(Req, State = #state{route = {<<"POST">>, undefined, undefined}}) ->
    % POST /room
    % For 201 Created
    {false, Req, State};
resource_exists(Req, State = #state{route = {_, RoomID, _Action}}) ->
    % /room/:roomid/:action
    {room:exists(RoomID), Req, State}.

allow_missing_post(Req, State = #state{route = {_, undefined, undefined}}) ->
    % /room
    {true, Req, State};
allow_missing_post(Req, State = #state{route = {_, _RoomID, _Action}}) ->
    % /room/:roomid/:action
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, accept_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, provide_json}], Req, State}.

%%%%%%%%%%%%%%%%%
%%% GET /room/:roomid
%%%%%%%%%%%%%%%%%
provide_json(
    Req,
    State = #state{route = {<<"GET">>, RoomID, undefined}, player_index = PlayerIndex}
) ->
    Body = room:state_to_json(RoomID, PlayerIndex),
    {Body, Req, State}.

%%%%%%%%%%%%%%%%%
%%% POST /room
%%%%%%%%%%%%%%%%%
accept_json(Req, State = #state{route = {<<"POST">>, undefined, undefined}}) ->
    {ok, ReqData, Req1} = cowboy_req:read_body(Req),
    case jsone:decode(ReqData) of
        #{<<"num_players">> := NumPlayers} when
            is_integer(NumPlayers), 2 =< NumPlayers, NumPlayers =< 4
        ->
            RoomID = room:create_one(NumPlayers),
            {{true, [<<"/room/">>, RoomID]}, Req1, State};
        _ ->
            {false, Req1, State}
    end;
%
%%%%%%%%%%%%%%%%%
%%% POST /room/:roomid/register
%%%%%%%%%%%%%%%%%
accept_json(Req, State = #state{route = {<<"POST">>, RoomID, register}, player_index = undefined}) ->
    % room exists
    true = room:exists(RoomID),

    case room:register_as_player(RoomID) of
        {ok, PlayerIndex} ->
            {ok, Req1} = cowboy_session:set({player_index, RoomID}, PlayerIndex, Req),
            {true, Req1, State};
        {error, _} ->
            {false, Req, State}
    end;
accept_json(Req, State = #state{route = {<<"POST">>, _RoomID, register}}) ->
    % already registered
    {false, Req, State};
%
%%%%%%%%%%%%%%%%%
%%% POST /room/:roomid/attack
%%%%%%%%%%%%%%%%%
accept_json(Req, State = #state{route = {<<"POST">>, RoomID, attack}, player_index = PlayerIndex}) ->
    % authorized & room exists
    {true, true} = {PlayerIndex =/= undefined, room:exists(RoomID)},

    {ok, ReqData, Req1} = cowboy_req:read_body(Req),
    case jsone:decode(ReqData) of
        #{
            <<"target_player">> := TargetPlayer,
            <<"target_hand_index">> := TargetIndex,
            <<"guess">> := Guess
        } when
            is_integer(TargetPlayer),
            is_integer(TargetIndex),
            is_integer(Guess),
            1 =< TargetPlayer,
            TargetPlayer =< 4,
            TargetPlayer =/= PlayerIndex,
            0 =< Guess,
            Guess < 24
        ->
            case room:attack(RoomID, PlayerIndex, TargetPlayer, TargetIndex, Guess) of
                {error, _Reason} ->
                    {false, Req1, State};
                {ok, Result} ->
                    Req2 = cowboy_req:set_resp_body(
                        jsone:encode(#{
                            result =>
                                case Result of
                                    success -> true;
                                    failure -> false
                                end
                        }),
                        Req1
                    ),
                    {true, Req2, State}
            end;
        _ ->
            {false, Req1, State}
    end;
%
%%%%%%%%%%%%%%%%%
%%% POST /room/:roomid/stay
%%%%%%%%%%%%%%%%%
accept_json(Req, State = #state{route = {<<"POST">>, RoomID, stay}, player_index = PlayerIndex}) ->
    % authorized & room exists
    {true, true} = {PlayerIndex =/= undefined, room:exists(RoomID)},

    case room:stay(RoomID, PlayerIndex) of
        {error, _Reason} ->
            {false, Req, State};
        ok ->
            {true, Req, State}
    end;
%
%%%%%%%%%%%%%%%%%
%%% POST /room/:roomid/choose_attacker_card
%%%%%%%%%%%%%%%%%
accept_json(
    Req,
    State = #state{route = {<<"POST">>, RoomID, choose_attacker_card}, player_index = PlayerIndex}
) ->
    % authorized & room exists
    {true, true} = {PlayerIndex =/= undefined, room:exists(RoomID)},

    {ok, ReqData, Req1} = cowboy_req:read_body(Req),
    case jsone:decode(ReqData) of
        #{<<"hand_index">> := HandIndex} when is_integer(HandIndex) ->
            case room:choose_attacker_card(RoomID, PlayerIndex, HandIndex) of
                ok -> {true, Req1, State};
                {error, _Reason} -> {false, Req1, State}
            end;
        _ ->
            {false, Req1, State}
    end.

%% Private functions
