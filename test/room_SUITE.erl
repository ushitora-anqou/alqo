-module(room_SUITE).
-compile(export_all).

all() -> [{group, session}].

groups() ->
    [
        {session, [parallel, {repeat, 10}], [
            create_register_get,
            attack_success_failure,
            websocket_observers,
            websocket_observers_stay,
            when_room_died,
            game_finished_winner,
            attack_and_stay
        ]}
    ].

init_per_group(session, Config) ->
    application:ensure_all_started(alqo),
    application:ensure_all_started(hackney),
    application:ensure_all_started(gun),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(session, _Config) ->
    application:stop(hackney),
    application:stop(gun),
    application:stop(alqo);
end_per_group(_, _Config) ->
    ok.

when_room_died(_Config) ->
    %% XXX test without sleep?
    RoomID = room_database:create_room(4),
    timer:sleep(10),
    ok =
        case room_database:get_pid(RoomID) of
            undefined ->
                error;
            Pid ->
                exit(Pid, kill),
                timer:sleep(10),
                case room_database:get_pid(RoomID) of
                    undefined -> error;
                    NewPid when NewPid =/= Pid -> ok
                end
        end.

create_register_get(_Config) ->
    RoomURL = create_room(4),

    {ok, 201, RespHd1, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Pl1Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd1),
    {ok, 400, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}, {<<"Cookie">>, Pl1Cookie}],
        ""
    ),

    {ok, 201, RespHd2, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Pl2Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd2),
    {ok, 201, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {ok, 201, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {ok, 400, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),

    % View from not logged-in users
    {ok, 200, _, ClientRef1} = hackney:get(RoomURL, [], "", []),
    {ok, BodySrc1} = hackney:body(ClientRef1),
    Body1 = jsone:decode(BodySrc1),
    #{
        <<"status">> := <<"playing">>,
        <<"board">> := #{
            <<"can_stay">> := false,
            <<"current_turn">> := 1,
            <<"next_turn">> := 2,
            <<"num_players">> := 4,
            <<"winner">> := null
        }
    } = Body1,
    Board1 = maps:get(<<"board">>, Body1),
    ok =
        case
            {maps:is_key(<<"your_player_index">>, Board1), maps:is_key(<<"your_hand">>, Board1),
                maps:is_key(<<"your_attacker_card_from_deck">>, Board1)}
        of
            {false, false, false} -> ok;
            _ -> error
        end,
    ok =
        case Board1 of
            #{<<"attacker_card">> := [1, 0]} -> ok;
            #{<<"attacker_card">> := [1, 1]} -> ok
        end,

    % View from logged-in user 1
    {ok, 200, _, ClientRef2} = hackney:get(RoomURL, [{<<"Cookie">>, Pl1Cookie}], "", []),
    {ok, BodySrc2} = hackney:body(ClientRef2),
    Body2 = jsone:decode(BodySrc2),
    #{
        <<"status">> := <<"playing">>,
        <<"board">> := #{
            <<"can_stay">> := false,
            <<"current_turn">> := 1,
            <<"next_turn">> := 2,
            <<"num_players">> := 4,
            <<"winner">> := null
        }
    } = Body2,
    Board2 = maps:get(<<"board">>, Body2),
    case Board2 of
        #{
            <<"attacker_card">> := [1, AttackerCard2],
            <<"your_player_index">> := 1,
            <<"your_hand">> := MyHand2,
            <<"your_attacker_card_from_deck">> := MyAttackerCardFromDeck
        } when MyHand2 =/= null, AttackerCard2 =:= MyAttackerCardFromDeck rem 2 ->
            ok
    end,

    % View from logged-in user 2
    {ok, 200, _, ClientRef3} = hackney:get(RoomURL, [{<<"Cookie">>, Pl2Cookie}], "", []),
    {ok, BodySrc3} = hackney:body(ClientRef3),
    Body3 = jsone:decode(BodySrc3),
    #{
        <<"status">> := <<"playing">>,
        <<"board">> := #{
            <<"can_stay">> := false,
            <<"current_turn">> := 1,
            <<"next_turn">> := 2,
            <<"num_players">> := 4,
            <<"winner">> := null
        }
    } = Body3,
    Board3 = maps:get(<<"board">>, Body3),
    case Board3 of
        #{
            <<"attacker_card">> := [1, AttackerCard3],
            <<"your_player_index">> := 2,
            <<"your_hand">> := MyHand3,
            <<"your_attacker_card_from_deck">> := null
        } when MyHand3 =/= null, AttackerCard3 =:= MyAttackerCardFromDeck rem 2 ->
            ok
    end,

    ok.

attack_success_failure(_Config) ->
    RoomURL = create_room(4),
    AttackURL = [RoomURL, <<"/attack">>],

    Pl1Cookie = register_as_player(RoomURL),
    Pl2Cookie = register_as_player(RoomURL),
    Pl3Cookie = register_as_player(RoomURL),
    Pl4Cookie = register_as_player(RoomURL),

    % Can't attack the player of the current turn.
    {ok, 400, _, _} = request(
        post,
        AttackURL,
        jsone:encode(#{
            target_player => 1,
            target_index => 1,
            guess => 1
        }),
        Pl1Cookie,
        []
    ),

    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),
    CardNum3_1 = get_hand_of(RoomURL, Pl3Cookie, 1),
    CardNum3_2 = get_hand_of(RoomURL, Pl3Cookie, 2),
    CardNum4_1 = get_hand_of(RoomURL, Pl4Cookie, 1),
    CardNum4_2 = get_hand_of(RoomURL, Pl4Cookie, 2),
    % Attack success
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 3, 1, CardNum3_1),
    % Attack failure
    #{<<"result">> := false} = attack(RoomURL, Pl1Cookie, 2, 1, card_different_from(CardNum2_1)),

    % Can't attack already opened card.
    {ok, 400, _, _} = request(
        post,
        AttackURL,
        jsone:encode(#{
            target_player => 3,
            target_index => 1,
            guess => CardNum3_1
        }),
        Pl2Cookie,
        []
    ),

    % Attack success
    #{<<"result">> := true} = attack(RoomURL, Pl2Cookie, 4, 1, CardNum4_1),
    % Attack success and player 3 lost.
    #{<<"result">> := true} = attack(RoomURL, Pl2Cookie, 3, 2, CardNum3_2),
    % Attack failure
    #{<<"result">> := false} = attack(RoomURL, Pl2Cookie, 4, 2, card_different_from(CardNum4_2)),
    % The next turn is 4, not 3
    #{<<"board">> := #{<<"current_turn">> := 4}} = get_room_state(RoomURL, Pl3Cookie),

    ok.

game_finished_winner(_Config) ->
    RoomURL = create_room(2),

    Pl1Cookie = register_as_player(RoomURL),
    Pl2Cookie = register_as_player(RoomURL),

    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),
    CardNum2_2 = get_hand_of(RoomURL, Pl2Cookie, 2),
    CardNum2_3 = get_hand_of(RoomURL, Pl2Cookie, 3),
    CardNum2_4 = get_hand_of(RoomURL, Pl2Cookie, 4),

    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 1, CardNum2_1),
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 2, CardNum2_2),
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 3, CardNum2_3),
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 4, CardNum2_4),

    #{
        <<"board">> := #{
            <<"winner">> := 1
        }
    } = get_room_state(RoomURL),

    % Stay in finished game will fail
    {ok, 400, _, _} = request(post, [RoomURL, <<"/stay">>], "", Pl1Cookie, []),

    ok.

attack_and_stay(_Config) ->
    RoomURL = create_room(2),
    StayURL = [RoomURL, <<"/stay">>],
    Pl1Cookie = register_as_player(RoomURL),
    Pl2Cookie = register_as_player(RoomURL),
    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),

    % Can't stay unless attack succeeds at least once
    {ok, 400, _, _} = request(post, StayURL, "", Pl1Cookie, []),

    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 1, CardNum2_1),
    % Stay success
    #{<<"result">> := true} = stay(RoomURL, Pl1Cookie),

    #{
        <<"board">> := #{
            <<"current_turn">> := 2
        }
    } = get_room_state(RoomURL),

    ok.

websocket_observers(_Config) ->
    RoomURL = create_room(2),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, [RoomURL, <<"/ws">>]),
    receive
        {gun_upgrade, _ConnPid, _StreamRef, [<<"websocket">>], _Headers} ->
            ok
    end,
    % Connection established

    Pl1Cookie = register_as_player(RoomURL),
    case ws_wait_json() of
        [<<"player_registered">>, 1] ->
            ok
    end,
    Pl2Cookie = register_as_player(RoomURL),
    case ws_wait_json() of
        [<<"player_registered">>, 2] ->
            ok
    end,

    case {get_room_state(RoomURL), ws_wait_json()} of
        {#{<<"board">> := Board1}, [<<"game_started">>, Board2]} when Board1 =:= Board2 ->
            ok
    end,
    % game started

    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),
    CardNum2_2 = get_hand_of(RoomURL, Pl2Cookie, 2),
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 1, CardNum2_1),
    case {get_room_state(RoomURL), ws_wait_json()} of
        {#{<<"board">> := Board3}, [
            <<"attacked">>,
            #{
                <<"board">> := Board4,
                <<"target_player">> := 2,
                <<"target_hand_index">> := 1,
                <<"guess">> := Guess2_1,
                <<"result">> := true
            }
        ]} when Board3 =:= Board4, CardNum2_1 =:= Guess2_1 ->
            ok
    end,
    #{<<"result">> := false} = attack(RoomURL, Pl1Cookie, 2, 2, card_different_from(CardNum2_2)),
    case {get_room_state(RoomURL), ws_wait_json()} of
        {#{<<"board">> := Board5}, [
            <<"attacked">>,
            #{
                <<"board">> := Board6,
                <<"target_player">> := 2,
                <<"target_hand_index">> := 2,
                <<"guess">> := Guess2_2,
                <<"result">> := false
            }
        ]} when Board5 =:= Board6 ->
            ok
    end,

    CardNum1_1 = get_hand_of(RoomURL, Pl1Cookie, 1),
    CardNum1_2 = get_hand_of(RoomURL, Pl1Cookie, 2),
    CardNum1_3 = get_hand_of(RoomURL, Pl1Cookie, 3),
    CardNum1_4 = get_hand_of(RoomURL, Pl1Cookie, 4),
    CardNum1_5 = get_hand_of(RoomURL, Pl1Cookie, 5),
    catch case attack(RoomURL, Pl2Cookie, 1, 1, CardNum1_1) of
        #{<<"result">> := true} ->
            [<<"attacked">>, _] = ws_wait_json()
    end,
    catch case attack(RoomURL, Pl2Cookie, 1, 2, CardNum1_2) of
        #{<<"result">> := true} ->
            [<<"attacked">>, _] = ws_wait_json()
    end,
    catch case attack(RoomURL, Pl2Cookie, 1, 3, CardNum1_3) of
        #{<<"result">> := true} ->
            [<<"attacked">>, _] = ws_wait_json()
    end,
    catch case attack(RoomURL, Pl2Cookie, 1, 4, CardNum1_4) of
        #{<<"result">> := true} ->
            [<<"attacked">>, _] = ws_wait_json()
    end,
    catch case attack(RoomURL, Pl2Cookie, 1, 5, CardNum1_5) of
        #{<<"result">> := true} ->
            [<<"attacked">>, _] = ws_wait_json()
    end,
    % game finished
    #{<<"board">> := #{<<"winner">> := 2}} = get_room_state(RoomURL),
    [<<"game_finished">>, 2] = ws_wait_json(),

    ok.

websocket_observers_stay(_Config) ->
    RoomURL = create_room(2),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, [RoomURL, <<"/ws">>]),
    receive
        {gun_upgrade, _ConnPid, _StreamRef, [<<"websocket">>], _Headers} ->
            ok
    end,
    % Connection established

    Pl1Cookie = register_as_player(RoomURL),
    case ws_wait_json() of
        [<<"player_registered">>, 1] ->
            ok
    end,
    Pl2Cookie = register_as_player(RoomURL),
    case ws_wait_json() of
        [<<"player_registered">>, 2] ->
            ok
    end,

    case {get_room_state(RoomURL), ws_wait_json()} of
        {#{<<"board">> := Board1}, [<<"game_started">>, Board2]} when Board1 =:= Board2 ->
            ok
    end,
    % game started

    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),
    CardNum2_2 = get_hand_of(RoomURL, Pl2Cookie, 2),
    #{<<"result">> := true} = attack(RoomURL, Pl1Cookie, 2, 1, CardNum2_1),
    case {get_room_state(RoomURL), ws_wait_json()} of
        {#{<<"board">> := Board3}, [
            <<"attacked">>,
            #{
                <<"board">> := Board4,
                <<"target_player">> := 2,
                <<"target_hand_index">> := 1,
                <<"guess">> := Guess2_1,
                <<"result">> := true
            }
        ]} when Board3 =:= Board4, CardNum2_1 =:= Guess2_1 ->
            ok
    end,

    #{<<"result">> := true} = stay(RoomURL, Pl1Cookie),
    case ws_wait_json() of
        [<<"stayed">>, _] -> ok
    end.

%% Helper functions
ws_wait_json() ->
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, Msg}} ->
            jsone:decode(Msg)
    after 5000 -> erlang:error(timeout)
    end.

create_room(NumPlayers) ->
    {ok, 200, _, ClientRef} = hackney:post(
        "http://localhost:8080/room",
        [{<<"Content-Type">>, <<"application/json">>}],
        jsone:encode(#{nplayers => NumPlayers}),
        [{follow_redirect, true}]
    ),
    RoomURL = hackney:location(ClientRef),
    RoomURL.

register_as_player(RoomURL) ->
    {ok, 201, RespHd, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd),
    Cookie.

request(Method, URL, Payload, Cookie, Options) ->
    HeadersWithoutCookie = [{<<"Content-Type">>, <<"application/json">>}],
    Headers =
        case Cookie of
            undefined -> HeadersWithoutCookie;
            V -> [{<<"Cookie">>, V} | HeadersWithoutCookie]
        end,
    hackney:request(Method, URL, Headers, Payload, Options).

get_room_state(RoomURL, Cookie) ->
    {ok, 200, _, ClientRef} = request(get, RoomURL, "", Cookie, []),
    {ok, Body} = hackney:body(ClientRef),
    jsone:decode(Body).

get_room_state(RoomURL) ->
    get_room_state(RoomURL, undefined).

attack(RoomURL, Cookie, TargetPlayer, TargetIndex, Guess) ->
    {ok, 200, _, ClientRef} = request(
        post,
        [RoomURL, <<"/attack">>],
        jsone:encode(#{
            target_player => TargetPlayer,
            target_index => TargetIndex,
            guess => Guess
        }),
        Cookie,
        []
    ),
    {ok, Body} = hackney:body(ClientRef),
    jsone:decode(Body).

stay(RoomURL, Cookie) ->
    {ok, 200, _, ClientRef} = request(
        post,
        [RoomURL, <<"/stay">>],
        "",
        Cookie,
        []
    ),
    {ok, Body} = hackney:body(ClientRef),
    jsone:decode(Body).

get_hand_of(RoomURL, Cookie, HandIndex) ->
    #{<<"board">> := #{<<"your_hand">> := Hand}} = get_room_state(RoomURL, Cookie),
    lists:nth(HandIndex, Hand).

card_different_from(23) -> 0;
card_different_from(N) -> N + 1.
