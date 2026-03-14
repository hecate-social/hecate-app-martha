%%% @doc PM: on entity recognized, detect cross-division conflicts.
%%% Subscribes to knowledge_graph_store for entity_recognized_v1 events.
%%% Checks if conflicting entities exist across divisions.
%%% Captures warnings as insights.
-module(on_entity_recognized_detect_conflicts).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"entity_recognized_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    spawn_link(fun() -> do_detect(Data) end),
    {ok, State}.

%% Internal

do_detect(Data) ->
    VentureId = gv(venture_id, Data),
    case VentureId of
        undefined -> ok;
        _ ->
            NewEntity = #{
                <<"name">> => gv(name, Data),
                <<"type">> => gv(entity_type, Data),
                <<"division_id">> => gv(division_id, Data)
            },
            case project_knowledge_graph_store:get_graph(VentureId) of
                {ok, Graph} ->
                    Conflicts = detect_conflicts:detect(NewEntity, Graph),
                    capture_conflicts(VentureId, Conflicts);
                {error, not_found} ->
                    ok
            end
    end.

capture_conflicts(_VentureId, []) ->
    ok;
capture_conflicts(VentureId, Conflicts) ->
    logger:info("[~s] found ~b cross-division conflicts for venture ~s",
               [?MODULE, length(Conflicts), VentureId]),
    lists:foreach(fun(Warning) ->
        Params = #{
            venture_id => VentureId,
            content => Warning,
            source_agent => <<"conflict_detector">>,
            source_session => <<"system">>,
            insight_type => <<"quality_warning">>
        },
        case capture_insight_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_capture_insight:dispatch(Cmd) of
                    {ok, _, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[~s] failed to dispatch conflict warning: ~p",
                                      [?MODULE, Reason])
                end;
            {error, Reason} ->
                logger:warning("[~s] failed to create conflict insight: ~p", [?MODULE, Reason])
        end
    end, Conflicts).

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
