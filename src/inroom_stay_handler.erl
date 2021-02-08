-module(inroom_stay_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    stay/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, stay}], Req, State}.

stay(Req, RoomID) ->
    case {cowboy_session:get({player_index, RoomID}, Req), room:exists(RoomID)} of
        {{_, Req1}, false} ->
            {false, Req1, RoomID};
        {{undefined, Req1}, true} ->
            {false, Req1, RoomID};
        {{PlayerIndex, Req1}, true} ->
            case room:stay(RoomID, PlayerIndex) of
                {error, _Reason} ->
                    {false, Req1, RoomID};
                ok ->
                    {true, Req1, RoomID}
            end
    end.

%% Private functions
