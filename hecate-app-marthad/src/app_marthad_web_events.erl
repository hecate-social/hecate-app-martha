%%% @doc Broadcast helper for pushing domain events to Martha web frontend.
%%%
%%% Stateless module. Any process can call broadcast/2 to push an event
%%% to all connected Martha SSE clients (via the martha_sse pg group).
%%% @end
-module(app_marthad_web_events).

-export([broadcast/2]).

-define(SCOPE, pg).
-define(GROUP, martha_sse).

-spec broadcast(binary(), map()) -> ok.
broadcast(EventType, Data) when is_binary(EventType), is_map(Data) ->
    JsonBin = iolist_to_binary(json:encode(Data)),
    Members = pg:get_members(?SCOPE, ?GROUP),
    [Pid ! {martha_event, EventType, JsonBin} || Pid <- Members],
    ok.
