%%% @doc Process Manager: vision_gate_rejected_v1 -> capture_insight
%%%
%%% When a vision gate is rejected, capture the rejection reason as a
%%% critical insight. Rejection reasons are some of the most valuable
%%% learning signals — they tell agents what NOT to do.
%%%
%%% Subscribes to: orchestration_store (vision_gate_rejected_v1)
%%% Dispatches to: knowledge_graph_store (capture_insight)
%%% @end
-module(on_vision_gate_rejected_capture_insight).

-behaviour(evoq_event_handler).

-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"vision_gate_rejected_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    VentureId = gf(venture_id, Data),
    spawn_link(fun() ->
        RejectedBy = coalesce(gf(rejected_by, Data), <<"unknown">>),
        Reason = coalesce(gf(rejection_reason, Data), <<"no reason provided">>),
        Content = <<"Vision gate REJECTED by ", RejectedBy/binary, ": ", Reason/binary>>,
        Params = #{
            venture_id => VentureId,
            content => Content,
            source_agent => <<"gate_reviewer">>,
            source_session => gf(session_id, Data),
            insight_type => <<"gate_rejection">>
        },
        case capture_insight_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_capture_insight:dispatch(Cmd) of
                    {ok, _, _} ->
                        logger:info("[KG-PM] Vision gate rejection captured for ~s", [VentureId]);
                    {error, CaptureErr} ->
                        logger:warning("[KG-PM] Failed to capture gate rejection: ~p", [CaptureErr])
                end;
            {error, NewErr} ->
                logger:warning("[KG-PM] Invalid gate rejection params: ~p", [NewErr])
        end
    end),
    {ok, State}.

gf(Key, Data) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

coalesce(undefined, Default) -> Default;
coalesce(Value, _Default) -> Value.
