-module(board_database_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

board_database_test_() ->
    [
        {"scenario1", ?setup(fun scenario1/1)}
    ].

start() ->
    board_database:start_link().

stop(_) ->
    board_database:stop().

scenario1(_) ->
    B1 = game:new_board(2),
    board_database:register_board(<<"scenario1">>, B1),
    B1_ = board_database:get_board_by_name(<<"scenario1">>),
    B2 = game:choose_attacker_card(B1),
    board_database:update_board(<<"scenario1">>, B1, B2),
    B2_ = board_database:get_board_by_name(<<"scenario1">>),
    [
        ?_assertEqual(B1, B1_),
        ?_assertEqual(B2, B2_)
    ].
