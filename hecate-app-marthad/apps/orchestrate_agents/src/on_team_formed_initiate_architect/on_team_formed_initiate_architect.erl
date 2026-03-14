%%% @doc Process manager: on team_formed, initiate architect.
%%% Reacts to team_formed_v1 and dispatches initiate_architect_v1 command.
-module(on_team_formed_initiate_architect).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"team_formed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    DivisionId = gv(division_id, Data),
    VentureId = gv(venture_id, Data),
    case {DivisionId, VentureId} of
        {undefined, _} ->
            logger:warning("[~s] missing division_id in event", [?MODULE]);
        {_, undefined} ->
            logger:warning("[~s] missing venture_id in event", [?MODULE]);
        _ ->
            CmdParams = #{
                venture_id => VentureId,
                division_id => DivisionId,
                tier => <<"T2">>,
                initiated_by => <<"system:pm">>
            },
            case initiate_architect_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_initiate_architect:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] initiated architect for division ~s",
                                       [?MODULE, DivisionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to initiate architect: ~p",
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
