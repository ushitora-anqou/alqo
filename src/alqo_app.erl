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
            {<<"/room">>, room_handler, undefined},
            {<<"/room/:roomid">>, room_handler, undefined},
            {<<"/room/:roomid/ws">>, ws_room, []},
            {<<"/room/:roomid/register">>, room_handler, register},
            {<<"/room/:roomid/attack">>, room_handler, attack},
            {<<"/room/:roomid/stay">>, room_handler, stay},
            {<<"/room/:roomid/choose_attacker_card">>, room_handler, choose_attacker_card}
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
