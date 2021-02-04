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

    {ok, 200, RespHd1, _} = hackney:post(
        [RoomURL, <<"/register">>],
        [{<<"Content-Type">>, <<"application/json">>}],
        "",
        [{follow_redirect, true}]
    ),
    %Pl1Cookie = dict:find(<<"set-cookie">>, RespHd1),
    {_, Pl1Cookie} = lists:keyfind(<<"set-cookie">>, 1, RespHd1),
    %Pl1Cookie = hackney_headers:get_value(<<"set-cookie">>, RespHd1),
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
            <<"your_player_index">> := null,
            <<"attacker_card">> := null
        }
    } = Body1,

    %% View from logged-in users
    %{ok, 200, _, ClientRef2} = hackney:get(RoomURL, [{<<"Cookie">>, Pl1Cookie}], "", []),
    %{ok, BodySrc2} = hackney:body(ClientRef2),
    %Body2 = jsone:decode(BodySrc2),
    %#{
    %    <<"status">> := <<"playing">>,
    %    <<"board">> := #{
    %        <<"can_stay">> := false,
    %        <<"current_turn">> := 1,
    %        <<"next_turn">> := 2,
    %        <<"num_players">> := 4,
    %        <<"winner">> := null,
    %        <<"your_player_index">> := 1,
    %        <<"attacker_card">> := null
    %    }
    %} = Body2,
    ok.
