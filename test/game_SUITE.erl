-module(game_SUITE).
-compile(export_all).

all() -> [run_eunit].

run_eunit(_Config) ->
    ok = eunit:test(game_test).
