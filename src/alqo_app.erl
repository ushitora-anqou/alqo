%%%-------------------------------------------------------------------
%% @doc alqo public API
%% @end
%%%-------------------------------------------------------------------

-module(alqo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    alqo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
