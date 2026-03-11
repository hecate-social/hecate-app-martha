-module(get_storming_by_id).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found | term()}.
get(DivisionId) ->
    case get_storming(DivisionId) of
        {ok, Storming} ->
            Aggregates = get_designed_aggregates(DivisionId),
            Events = get_designed_events(DivisionId),
            Desks = get_planned_desks(DivisionId),
            Dependencies = get_planned_dependencies(DivisionId),
            {ok, Storming#{
                designed_aggregates => Aggregates,
                designed_events => Events,
                planned_desks => Desks,
                planned_dependencies => Dependencies
            }};
        Error ->
            Error
    end.

get_storming(DivisionId) ->
    Sql = "SELECT division_id, venture_id, context_name, status, status_label, "
          "initiated_at, initiated_by "
          "FROM division_stormings WHERE division_id = ?1",
    case project_division_stormings_store:query(Sql, [DivisionId]) of
        {ok, [[DId, VId, CName, Status, SLabel, IAt, IBy]]} ->
            {ok, #{
                division_id => DId,
                venture_id => VId,
                context_name => CName,
                status => Status,
                status_label => SLabel,
                initiated_at => IAt,
                initiated_by => IBy
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

get_designed_aggregates(DivisionId) ->
    Sql = "SELECT aggregate_name, description, stream_prefix, fields, designed_at "
          "FROM designed_aggregates WHERE division_id = ?1",
    case project_division_stormings_store:query(Sql, [DivisionId]) of
        {ok, Rows} ->
            [#{aggregate_name => N, description => D, stream_prefix => SP,
               fields => F, designed_at => DA} || [N, D, SP, F, DA] <- Rows];
        _ -> []
    end.

get_designed_events(DivisionId) ->
    Sql = "SELECT event_name, description, aggregate_name, fields, designed_at "
          "FROM designed_events WHERE division_id = ?1",
    case project_division_stormings_store:query(Sql, [DivisionId]) of
        {ok, Rows} ->
            [#{event_name => N, description => D, aggregate_name => AN,
               fields => F, designed_at => DA} || [N, D, AN, F, DA] <- Rows];
        _ -> []
    end.

get_planned_desks(DivisionId) ->
    Sql = "SELECT desk_name, description, department, commands, planned_at "
          "FROM planned_desks WHERE division_id = ?1",
    case project_division_stormings_store:query(Sql, [DivisionId]) of
        {ok, Rows} ->
            [#{desk_name => N, description => D, department => Dep,
               commands => C, planned_at => PA} || [N, D, Dep, C, PA] <- Rows];
        _ -> []
    end.

get_planned_dependencies(DivisionId) ->
    Sql = "SELECT dependency_id, from_desk, to_desk, dep_type, planned_at "
          "FROM planned_dependencies WHERE division_id = ?1",
    case project_division_stormings_store:query(Sql, [DivisionId]) of
        {ok, Rows} ->
            [#{dependency_id => Id, from_desk => FD, to_desk => TD,
               dep_type => DT, planned_at => PA} || [Id, FD, TD, DT, PA] <- Rows];
        _ -> []
    end.
