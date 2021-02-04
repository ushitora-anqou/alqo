-module(room_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, create_room/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_room(NumPlayers) ->
    supervisor:start_child(?MODULE, [NumPlayers]).

init([]) ->
    {ok,
        {{simple_one_for_one, 0, 1}, [
            {room, {room, start_link, []}, transient, infinity, worker, [room]}
        ]}}.
