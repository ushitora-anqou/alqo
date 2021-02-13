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
            websocket_players,
            when_room_died,
            game_finished_winner,
            attack_and_stay,
            choose_attacker_card_from_hand
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
    RoomID = room:create_one(4),
    timer:sleep(10),
    ok =
        case gproc:where({n, l, {alqo_room, RoomID}}) of
            undefined ->
                error;
            Pid ->
                exit(Pid, kill),
                timer:sleep(20),
                case gproc:where({n, l, {alqo_room, RoomID}}) of
                    undefined -> error;
                    NewPid when NewPid =/= Pid -> ok
                end
        end.

create_register_get(_Config) ->
    RoomURL = create_room(4),

    {ok, 204, RespHd1, _} = hackney:post(
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

    {ok, 204, RespHd2, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Pl2Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd2),
    {ok, 204, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {ok, 204, _, _} = hackney:post(
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
    4 = length(maps:get(<<"hands">>, Board1)),
    2 = length(lists:nth(1, maps:get(<<"hands">>, Board1))),
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
            target_hand_index => 1,
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
            target_hand_index => 1,
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
    stay(RoomURL, Pl1Cookie),

    #{
        <<"board">> := #{
            <<"current_turn">> := 2
        }
    } = get_room_state(RoomURL),

    ok.

choose_attacker_card_from_hand(_Config) ->
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
    [<<"player_registered">>, 1] = ws_wait_json(),
    Pl2Cookie = register_as_player(RoomURL),
    [<<"player_registered">>, 2] = ws_wait_json(),

    [<<"game_started">>, _Board] = ws_wait_json(),
    % game started

    lists:foreach(
        fun(_I) ->
            TargetHandIndex1 = find_hidden_hand(RoomURL, 2),
            TargetNum1 = get_hand_of(RoomURL, Pl2Cookie, TargetHandIndex1),
            #{<<"result">> := false} = attack(
                RoomURL,
                Pl1Cookie,
                2,
                TargetHandIndex1,
                card_different_from(TargetNum1)
            ),
            [<<"attacked">>, _] = ws_wait_json(),

            TargetHandIndex2 = find_hidden_hand(RoomURL, 1),
            TargetNum2 = get_hand_of(RoomURL, Pl1Cookie, TargetHandIndex2),
            #{<<"result">> := false} = attack(
                RoomURL,
                Pl2Cookie,
                1,
                TargetHandIndex2,
                card_different_from(TargetNum2)
            ),
            [<<"attacked">>, _] = ws_wait_json()
        end,
        lists:seq(1, 8)
    ),

    #{<<"board">> := #{<<"attacker_card">> := null, <<"your_attacker_card_from_deck">> := null}} = get_room_state(
        RoomURL,
        Pl1Cookie
    ),
    HI = find_hidden_hand(RoomURL, 1),
    choose_attacker_card(RoomURL, Pl1Cookie, HI),
    [<<"attacker_card_chosen">>, HI] = ws_wait_json(),
    #{<<"board">> := #{<<"attacker_card">> := [2, HI]}} = get_room_state(RoomURL),
    TargetHandIndex3 = find_hidden_hand(RoomURL, 2),
    TargetNum3 = get_hand_of(RoomURL, Pl2Cookie, TargetHandIndex3),
    #{<<"result">> := false} = attack(
        RoomURL,
        Pl1Cookie,
        2,
        TargetHandIndex3,
        card_different_from(TargetNum3)
    ),
    #{<<"board">> := #{<<"attacker_card">> := null, <<"your_attacker_card_from_deck">> := null}} = get_room_state(
        RoomURL,
        Pl2Cookie
    ),

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
                <<"guess">> := _Guess2_2,
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

    stay(RoomURL, Pl1Cookie),
    case ws_wait_json() of
        [<<"stayed">>, _] -> ok
    end.

websocket_players(_Config) ->
    RoomURL = create_room(2),
    Pl1Cookie = register_as_player(RoomURL),

    {ok, ConnPid1} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid1),
    gun:ws_upgrade(ConnPid1, [RoomURL, <<"/ws">>], [{<<"Cookie">>, Pl1Cookie}]),
    receive
        {gun_upgrade, _ConnPid1, _StreamRef1, [<<"websocket">>], _Headers1} ->
            ok
    end,
    % Connection for player 1 has been established

    Pl2Cookie = register_as_player(RoomURL),
    {ok, ConnPid2} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid2),
    gun:ws_upgrade(ConnPid2, [RoomURL, <<"/ws">>], [{<<"Cookie">>, Pl2Cookie}]),
    receive
        {gun_upgrade, _ConnPid2, _StreamRef2, [<<"websocket">>], _Headers2} ->
            ok
    end,
    % Connection for player 2 has been established

    [<<"player_registered">>, 2] = ws_wait_json(ConnPid1),
    [<<"game_started">>, _Board] = ws_wait_json(ConnPid1),

    % Event your_hand
    case {get_room_state(RoomURL, Pl1Cookie), ws_wait_json(ConnPid1)} of
        {#{<<"board">> := #{<<"your_hand">> := MyHand1}}, [<<"your_hand">>, MyHand2]} when
            MyHand1 =:= MyHand2
        ->
            ok
    end,

    % Event your_turn
    case {get_room_state(RoomURL, Pl1Cookie), ws_wait_json(ConnPid1)} of
        {#{<<"board">> := #{<<"your_attacker_card_from_deck">> := MyAttackerCard1}}, [
            <<"your_turn">>,
            MyAttackerCard2
        ]} when MyAttackerCard1 =:= MyAttackerCard2 ->
            ok
    end,

    CardNum2_1 = get_hand_of(RoomURL, Pl2Cookie, 1),
    #{<<"result">> := false} = attack(RoomURL, Pl1Cookie, 2, 1, card_different_from(CardNum2_1)),
    [<<"attacked">>, #{<<"result">> := false}] = ws_wait_json(ConnPid1),
    [<<"attacked">>, #{<<"result">> := false}] = ws_wait_json(ConnPid2),

    % Event your_turn for player 2
    case {get_room_state(RoomURL, Pl2Cookie), ws_wait_json(ConnPid2)} of
        {#{<<"board">> := #{<<"your_attacker_card_from_deck">> := MyAttackerCard3}}, [
            <<"your_turn">>,
            MyAttackerCard4
        ]} when MyAttackerCard3 =:= MyAttackerCard4 ->
            ok
    end,

    ok.

%% Helper functions
ws_wait_json(ConnPid) ->
    receive
        {gun_ws, ConnPid_, _StreamRef, {text, Msg}} when
            ConnPid =:= undefined; ConnPid =:= ConnPid_
        ->
            jsone:decode(Msg)
    after 5000 -> erlang:error(timeout)
    end.
ws_wait_json() -> ws_wait_json(undefined).

create_room(NumPlayers) ->
    {ok, 200, _RespHd, ClientRef} = hackney:post(
        "http://localhost:8080/api/v1/room",
        [{<<"Content-Type">>, <<"application/json">>}],
        jsone:encode(#{num_players => NumPlayers})
    ),
    {ok, Body} = hackney:body(ClientRef),
    #{<<"roomid">> := RoomID} = jsone:decode(Body),
    [<<"http://localhost:8080/api/v1/room/">>, RoomID].

register_as_player(RoomURL) ->
    {ok, 204, RespHd, _} = hackney:post(
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
            target_hand_index => TargetIndex,
            guess => Guess
        }),
        Cookie,
        []
    ),
    {ok, Body} = hackney:body(ClientRef),
    jsone:decode(Body).

stay(RoomURL, Cookie) ->
    {ok, 204, _, _ClientRef} = request(
        post,
        [RoomURL, <<"/stay">>],
        "",
        Cookie,
        []
    ).

choose_attacker_card(RoomURL, Cookie, HandIndex) ->
    {ok, 204, _, _ClientRef} = request(
        post,
        [RoomURL, <<"/choose_attacker_card">>],
        jsone:encode(#{hand_index => HandIndex}),
        Cookie,
        []
    ).

get_hand_of(RoomURL, Cookie, HandIndex) ->
    #{<<"board">> := #{<<"your_hand">> := Hand}} = get_room_state(RoomURL, Cookie),
    lists:nth(HandIndex, Hand).

find_hidden_hand(RoomURL, TargetPlayer) ->
    #{<<"board">> := #{<<"hands">> := Hands}} = get_room_state(RoomURL),
    Hand = lists:nth(TargetPlayer, Hands),
    case util:find_first_index(fun([_, H]) -> H end, Hand) of
        false ->
            throw(invalid_board);
        HandIndex ->
            [_Color, true] = lists:nth(HandIndex, Hand),
            HandIndex
    end.

card_different_from(23) -> 0;
card_different_from(N) -> N + 1.
