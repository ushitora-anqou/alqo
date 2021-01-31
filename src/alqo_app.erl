%%%-------------------------------------------------------------------
%% @doc alqo public API
%% @end
%%%-------------------------------------------------------------------

-module(alqo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {<<"localhost">>, [
            {<<"/room">>, room_handler, []},
            {<<"/room/:roomid">>, inroom_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        hello_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    alqo_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(hello_listener).

%% internal functions
