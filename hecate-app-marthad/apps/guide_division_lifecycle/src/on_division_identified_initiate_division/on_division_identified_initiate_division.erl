%%% @doc Process manager: on division_identified, initiate division.
%%% Reacts to division_identified_v1 events.
-module(on_division_identified_initiate_division).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"division_identified_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    DivisionId = gv(division_id, Data),
    VentureId = gv(venture_id, Data),
    ContextName = gv(context_name, Data),
    case {DivisionId, VentureId, ContextName} of
        {undefined, _, _} ->
            logger:warning("[~s] missing division_id in event", [?MODULE]);
        {_, undefined, _} ->
            logger:warning("[~s] missing venture_id in event", [?MODULE]);
        {_, _, undefined} ->
            logger:warning("[~s] missing context_name in event", [?MODULE]);
        _ ->
            CmdParams = #{
                division_id => DivisionId,
                venture_id => VentureId,
                context_name => ContextName,
                initiated_by => <<"system:pm">>
            },
            case initiate_division_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_initiate_division:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] initiated division ~s", [?MODULE, DivisionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to initiate division ~s: ~p",
                                          [?MODULE, DivisionId, Reason])
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
