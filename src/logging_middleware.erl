-module(logging_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    logger:debug("~p~n", [Req]),
    {ok, Req, Env}.
