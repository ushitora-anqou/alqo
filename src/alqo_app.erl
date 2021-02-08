%%%-------------------------------------------------------------------
%% @doc alqo public API
%% @end
%%%-------------------------------------------------------------------

-module(alqo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cowboy_session:start(),
    Dispatch = cowboy_router:compile([
        {<<"localhost">>, [
            {<<"/">>, cowboy_static, {priv_file, alqo, "static/index.html"}},
            {<<"/room">>, room_handler, []},
            {<<"/room/:roomid">>, inroom_handler, []},
            {<<"/room/:roomid/ws">>, ws_room, []},
            {<<"/room/:roomid/register">>, inroom_register_handler, []},
            {<<"/room/:roomid/attack">>, inroom_attack_handler, []},
            {<<"/room/:roomid/stay">>, inroom_stay_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        hello_listener,
        [{port, 8080}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_session, cowboy_router, cowboy_handler]
        }
    ),
    alqo_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(hello_listener).

%% internal functions
