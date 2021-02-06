-module(inroom_attack_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    attack_target/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, attack_target}], Req, State}.

attack_target(Req, RoomID) ->
    case {cowboy_session:get({player_index, RoomID}, Req), room_database:get_pid(RoomID)} of
        {{_, Req1}, undefined} ->
            {false, Req1, RoomID};
        {{undefined, Req1}, _} ->
            {false, Req1, RoomID};
        {{PlayerIndex, Req1}, Pid} ->
            {ok, ReqData, Req2} = cowboy_req:read_body(Req1),
            case jsone:decode(ReqData) of
                #{
                    <<"target_player">> := TargetPlayer,
                    <<"target_index">> := TargetIndex,
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
                    attack_target_impl(
                        Req2,
                        RoomID,
                        Pid,
                        PlayerIndex,
                        TargetPlayer,
                        TargetIndex,
                        Guess
                    );
                _ ->
                    {false, Req2, RoomID}
            end
    end.

%% Private functions

attack_target_impl(Req, RoomID, Pid, PlayerIndex, TargetPlayer, TargetIndex, Guess) ->
    case room:attack(Pid, PlayerIndex, TargetPlayer, TargetIndex, Guess) of
        {error, Reason} ->
            {false, Req, RoomID};
        {ok, Result} ->
            Req1 = cowboy_req:set_resp_body(
                jsone:encode(#{
                    result =>
                        case Result of
                            success -> true;
                            failure -> false
                        end
                }),
                Req
            ),
            {true, Req1, RoomID}
    end.
