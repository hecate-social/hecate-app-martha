%%% @doc Process manager: on division_identified_v1, form a division team.
%%% Creates a team with planned roles for the standard agent lineup.
-module(on_division_identified_form_team).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(DEFAULT_PLANNED_ROLES, [
    <<"stormer">>,
    <<"architect">>,
    <<"erlang_coder">>,
    <<"svelte_coder">>,
    <<"sql_coder">>,
    <<"tester">>,
    <<"reviewer">>
]).

interested_in() -> [<<"division_identified_v1">>].

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
                division_id => DivisionId,
                venture_id => VentureId,
                planned_roles => ?DEFAULT_PLANNED_ROLES,
                formed_by => <<"system:pm">>
            },
            case form_team_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_form_team:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] formed team for division ~s",
                                       [?MODULE, DivisionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to form team for ~s: ~p",
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
