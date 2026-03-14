%%% @doc Process manager: on vision_gate_passed, initiate explorer.
%%% Reacts to vision_gate_passed_v1 events.
-module(on_vision_gate_passed_initiate_explorer).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"vision_gate_passed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    VentureId = gv(venture_id, Data),
    case VentureId of
        undefined ->
            logger:warning("[~s] missing venture_id in event", [?MODULE]);
        _ ->
            CmdParams = #{
                venture_id => VentureId,
                tier => <<"T1">>,
                initiated_by => <<"system:pm">>,
                input_context => gv(notation_output, Data)
            },
            case initiate_explorer_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_initiate_explorer:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] initiated explorer session", [?MODULE]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to initiate explorer: ~p",
                                          [?MODULE, Reason])
                    end;
                {error, Reason} ->
                    logger:warning("[~s] failed to create command: ~p", [?MODULE, Reason])
            end
    end,
    {ok, State}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
