-module(board_database).
-include_lib("stdlib/include/ms_transform.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0, register_board/2, get_board_by_name/1, update_board/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

register_board(Name, Board) ->
    gen_server:call(?MODULE, {register, Name, Board}).

get_board_by_name(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Board}] -> Board;
        [] -> undefined
    end.

update_board(Name, OldBoard, NewBoard) ->
    gen_server:call(?MODULE, {update, Name, OldBoard, NewBoard}).

%%% GEN_SERVER CALLBACKS
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.

handle_call({register, Name, Board}, _From, Tid) ->
    MatchSpec = ets:fun2ms(fun({N, B}) when N =:= Name -> {N, B} end),
    case ets:select(Tid, MatchSpec) of
        [] ->
            ets:insert(Tid, {Name, Board}),
            {reply, ok, Tid};
        [{_Name, _} | _] ->
            {reply, {error, name_taken}, Tid}
    end;
handle_call({update, Name, OldBoard, NewBoard}, _From, Tid) ->
    % 現在OldBoardであるときに限って、これをNewBoardに更新する。
    % FIXME: 絶対もっと良い方法ある。
    MatchSpec = ets:fun2ms(fun({N, B}) when N =:= Name -> {N, B} end),
    case ets:select(Tid, MatchSpec) of
        [] ->
            {reply, {error, not_registered_name}, Tid};
        [{Name, OldBoard} | _] ->
            ets:insert(Tid, {Name, NewBoard}),
            {reply, ok, Tid};
        _ ->
            {reply, {error, already_updated}, Tid}
    end;
handle_call(stop, _From, Tid) ->
    ets:delete(Tid),
    {stop, normal, ok, Tid};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
