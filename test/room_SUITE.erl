-module(room_SUITE).
-compile(export_all).

all() -> [{group, session}].

groups() -> [{session, [], [create_register_get]}].

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

    {ok, 303, RespHd1, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Pl1Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd1),
    {ok, 400, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}, {<<"Cookie">>, Pl1Cookie}],
        "",
        [{follow_redirect, true}]
    ),

    {ok, 303, RespHd2, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        ""
    ),
    {_, Pl2Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd2),
    {ok, 200, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        "",
        [{follow_redirect, true}]
    ),
    {ok, 200, _, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        "",
        [{follow_redirect, true}]
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
