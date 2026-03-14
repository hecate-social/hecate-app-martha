%%% @doc Process manager: on venture_initiated, initiate mentor.
%%% Reacts to venture_initiated_v1 events.
-module(on_venture_initiated_initiate_mentor).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"venture_initiated_v1">>].

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
                input_context => gv(description, Data)
            },
            case initiate_mentor_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_initiate_mentor:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] initiated mentor session", [?MODULE]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to initiate mentor: ~p",
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
