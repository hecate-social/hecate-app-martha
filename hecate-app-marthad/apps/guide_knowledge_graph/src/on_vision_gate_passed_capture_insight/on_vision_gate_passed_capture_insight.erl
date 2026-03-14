%%% @doc Process Manager: vision_gate_passed_v1 -> capture_insight
%%%
%%% When a vision gate is passed (human approval), capture the approval
%%% as an insight. The notation_output from the gate event is also extracted
%%% for knowledge if present.
%%%
%%% Subscribes to: orchestration_store (vision_gate_passed_v1)
%%% Dispatches to: knowledge_graph_store (capture_insight)
%%% @end
-module(on_vision_gate_passed_capture_insight).

-behaviour(evoq_event_handler).

-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"vision_gate_passed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    VentureId = gf(venture_id, Data),
    spawn_link(fun() ->
        %% Capture the gate pass as an insight
        PassedBy = coalesce(gf(passed_by, Data), <<"unknown">>),
        Content = <<"Vision gate passed by ", PassedBy/binary>>,
        Params = #{
            venture_id => VentureId,
            content => Content,
            source_agent => <<"gate_reviewer">>,
            source_session => gf(session_id, Data),
            insight_type => <<"gate_decision">>
        },
        case capture_insight_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_capture_insight:dispatch(Cmd) of
                    {ok, _, _} ->
                        logger:info("[KG-PM] Vision gate pass captured for ~s", [VentureId]);
                    {error, Reason} ->
                        logger:warning("[KG-PM] Failed to capture gate pass: ~p", [Reason])
                end;
            {error, Reason} ->
                logger:warning("[KG-PM] Invalid gate pass params: ~p", [Reason])
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
