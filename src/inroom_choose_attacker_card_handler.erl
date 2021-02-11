-module(inroom_choose_attacker_card_handler).
-behaviour(cowboy_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    choose_attacker_card/2
]).

init(Req, _State) ->
    RoomID = cowboy_req:binding(roomid, Req),
    {cowboy_rest, Req, RoomID}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, choose_attacker_card}], Req, State}.

choose_attacker_card(Req, RoomID) ->
    case {cowboy_session:get({player_index, RoomID}, Req), room:exists(RoomID)} of
        {{_, Req1}, false} ->
            {false, Req1, RoomID};
        {{undefined, Req1}, true} ->
            {false, Req1, RoomID};
        {{PlayerIndex, Req1}, true} ->
            {ok, ReqData, Req2} = cowboy_req:read_body(Req1),
            case jsone:decode(ReqData) of
                #{<<"hand_index">> := HandIndex} when is_integer(HandIndex) ->
                    case room:choose_attacker_card(RoomID, PlayerIndex, HandIndex) of
                        ok -> {true, Req2, RoomID};
                        {error, _Reason} -> {false, Req2, RoomID}
                    end;
                _ ->
                    {false, Req2, RoomID}
            end
    end.
