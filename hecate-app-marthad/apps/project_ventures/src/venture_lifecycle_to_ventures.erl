%%% @doc Merged projection: venture lifecycle events -> ventures ETS table.
%%%
%%% Single evoq_projection handling all events that update the ventures
%%% read model. Sequential processing through one gen_server mailbox
%%% guarantees no race conditions between event handlers.
%%% @end
-module(venture_lifecycle_to_ventures).
-behaviour(evoq_projection).

-include_lib("guide_venture_lifecycle/include/venture_lifecycle_status.hrl").

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_ventures).

interested_in() ->
    [<<"venture_initiated_v1">>,
     <<"vision_refined_v1">>,
     <<"vision_submitted_v1">>,
     <<"discovery_started_v1">>,
     <<"discovery_paused_v1">>,
     <<"discovery_resumed_v1">>,
     <<"discovery_completed_v1">>,
     <<"venture_archived_v1">>,
     <<"venture_repo_scaffolded_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

%% --- venture_initiated_v1: INSERT new venture ---

do_project(<<"venture_initiated_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    Status = evoq_bit_flags:set(0, ?VL_INITIATED),
    Venture = #{
        venture_id  => VentureId,
        name        => gf(name, Data),
        brief       => gf(brief, Data),
        status      => Status,
        status_label => evoq_bit_flags:to_string(Status, ?VL_FLAG_MAP),
        repos       => gf(repos, Data, []),
        skills      => gf(skills, Data, []),
        context_map => gf(context_map, Data, #{}),
        repo_path   => undefined,
        initiated_at => gf(initiated_at, Data),
        initiated_by => gf(initiated_by, Data)
    },
    {ok, RM2} = evoq_read_model:put(VentureId, Venture, RM),
    {ok, State, RM2};

%% --- vision_refined_v1: UPDATE fields, set VISION_REFINED flag ---

do_project(<<"vision_refined_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    case evoq_read_model:get(VentureId, RM) of
        {ok, V} ->
            NewStatus = evoq_bit_flags:set(maps:get(status, V, 0), ?VL_VISION_REFINED),
            Updated = V#{
                brief       => coalesce(gf(brief, Data), maps:get(brief, V)),
                repos       => coalesce(gf(repos, Data), maps:get(repos, V)),
                skills      => coalesce(gf(skills, Data), maps:get(skills, V)),
                context_map => coalesce(gf(context_map, Data), maps:get(context_map, V)),
                status      => NewStatus,
                status_label => evoq_bit_flags:to_string(NewStatus, ?VL_FLAG_MAP)
            },
            {ok, RM2} = evoq_read_model:put(VentureId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

%% --- vision_submitted_v1: set SUBMITTED flag ---

do_project(<<"vision_submitted_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        evoq_bit_flags:set(S, ?VL_SUBMITTED)
    end, State, RM);

%% --- discovery_started_v1: set DISCOVERING flag ---

do_project(<<"discovery_started_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        evoq_bit_flags:set(S, ?VL_DISCOVERING)
    end, State, RM);

%% --- discovery_paused_v1: unset DISCOVERING, set DISCOVERY_PAUSED ---

do_project(<<"discovery_paused_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        S1 = evoq_bit_flags:unset(S, ?VL_DISCOVERING),
        evoq_bit_flags:set(S1, ?VL_DISCOVERY_PAUSED)
    end, State, RM);

%% --- discovery_resumed_v1: unset DISCOVERY_PAUSED, set DISCOVERING ---

do_project(<<"discovery_resumed_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        S1 = evoq_bit_flags:unset(S, ?VL_DISCOVERY_PAUSED),
        evoq_bit_flags:set(S1, ?VL_DISCOVERING)
    end, State, RM);

%% --- discovery_completed_v1: unset DISCOVERING, set DISCOVERY_COMPLETED ---

do_project(<<"discovery_completed_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        S1 = evoq_bit_flags:unset(S, ?VL_DISCOVERING),
        evoq_bit_flags:set(S1, ?VL_DISCOVERY_COMPLETED)
    end, State, RM);

%% --- venture_archived_v1: set ARCHIVED flag ---

do_project(<<"venture_archived_v1">>, Data, State, RM) ->
    update_status(gf(venture_id, Data), fun(S) ->
        evoq_bit_flags:set(S, ?VL_ARCHIVED)
    end, State, RM);

%% --- venture_repo_scaffolded_v1: set VISION_REFINED + SUBMITTED, update fields ---

do_project(<<"venture_repo_scaffolded_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    case evoq_read_model:get(VentureId, RM) of
        {ok, V} ->
            NewStatus = ?VL_INITIATED bor ?VL_VISION_REFINED bor ?VL_SUBMITTED,
            Updated = V#{
                brief       => coalesce(gf(brief, Data), maps:get(brief, V)),
                repo_path   => gf(repo_path, Data),
                status      => NewStatus,
                status_label => evoq_bit_flags:to_string(NewStatus, ?VL_FLAG_MAP)
            },
            {ok, RM2} = evoq_read_model:put(VentureId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

%% --- Unknown ---

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

%%====================================================================
%% Internal
%%====================================================================

update_status(VentureId, StatusFun, State, RM) ->
    case evoq_read_model:get(VentureId, RM) of
        {ok, V} ->
            OldStatus = maps:get(status, V, 0),
            NewStatus = StatusFun(OldStatus),
            Updated = V#{
                status => NewStatus,
                status_label => evoq_bit_flags:to_string(NewStatus, ?VL_FLAG_MAP)
            },
            {ok, RM2} = evoq_read_model:put(VentureId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(#{<<"event_type">> := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, Default)
    end.

coalesce(undefined, Existing) -> Existing;
coalesce(null, Existing) -> Existing;
coalesce(New, _Existing) -> New.
