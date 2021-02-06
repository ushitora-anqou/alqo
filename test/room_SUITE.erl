-module(room_SUITE).
-compile(export_all).

all() -> [{group, session}].

groups() -> [{session, [parallel, {repeat, 10}], [create_register_get, attack_success_failure]}].

init_per_group(session, Config) ->
    application:ensure_all_started(alqo),
    application:ensure_all_started(hackney),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(session, _Config) ->
    ok;
end_per_group(_, _Config) ->
    ok.

create_register_get(_Config) ->
    {ok, 200, _, ClientRef} = hackney:post(
        "http://localhost:8080/room",
        [{<<"Content-Type">>, <<"application/json">>}],
        jsone:encode(#{nplayers => 4}),
        [{follow_redirect, true}]
    ),
    RoomURL = hackney:location(ClientRef),

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
            <<"winner">> := null,
            <<"attacker_card">> := [1, _]
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
            <<"winner">> := null,
            <<"attacker_card">> := [1, _]
        }
    } = Body2,
    case Body2 of
        #{
            <<"board">> := #{
                <<"your_player_index">> := 1,
                <<"your_hand">> := MyHand2,
                <<"your_attacker_card_from_deck">> := MyAttackerCardFromDeck
            }
        } when MyHand2 =/= null, MyAttackerCardFromDeck =/= null ->
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
            <<"winner">> := null,
            <<"attacker_card">> := [1, _]
        }
    } = Body3,
    case Body3 of
        #{
            <<"board">> := #{
                <<"your_player_index">> := 2,
                <<"your_hand">> := MyHand3,
                <<"your_attacker_card_from_deck">> := null
            }
        } when MyHand3 =/= null ->
            ok
    end,

    ok.

attack_success_failure(_Config) ->
    {ok, 200, _, ClientRef} = hackney:post(
        "http://localhost:8080/room",
        [{<<"Content-Type">>, <<"application/json">>}],
        jsone:encode(#{nplayers => 4}),
        [{follow_redirect, true}]
    ),
    RoomURL = hackney:location(ClientRef),
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

%% Helper functions
register_as_player(RoomURL) ->
    {ok, 201, RespHd, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd),
    Cookie.

request(Method, URL, Payload, Cookie, Options) ->
    hackney:request(
        Method,
        URL,
        [{<<"Content-Type">>, <<"application/json">>}, {<<"Cookie">>, Cookie}],
        Payload,
        Options
    ).

get_room_state(RoomURL, Cookie) ->
    %{ok, 200, _, ClientRef} = hackney:get(RoomURL, [{<<"Cookie">>, Cookie}], "", []),
    {ok, 200, _, ClientRef} = request(get, RoomURL, "", Cookie, []),
    {ok, Body} = hackney:body(ClientRef),
    jsone:decode(Body).

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

get_hand_of(RoomURL, Cookie, HandIndex) ->
    #{<<"board">> := #{<<"your_hand">> := Hand}} = get_room_state(RoomURL, Cookie),
    lists:nth(HandIndex, Hand).

card_different_from(23) -> 0;
card_different_from(N) -> N + 1.
