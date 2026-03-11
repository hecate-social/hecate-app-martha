%%% @doc Projection: planning_submitted_v1 -> division_plannings table (set SUBMITTED flag)
-module(planning_submitted_v1_to_sqlite_division_plannings).

-include_lib("guide_division_planning/include/planning_status.hrl").

-export([project/1]).

project(Event) ->
    DivisionId = get(division_id, Event),
    SubmittedAt = get(submitted_at, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, DivisionId]),
    case project_division_plannings_store:query(
        "SELECT status FROM division_plannings WHERE division_id = ?1", [DivisionId]) of
        {ok, [[CurrentStatus]]} ->
            NewStatus = evoq_bit_flags:set(CurrentStatus, ?PLANNING_SUBMITTED),
            StatusLabel = evoq_bit_flags:to_string(NewStatus, ?PLANNING_FLAG_MAP),
            project_division_plannings_store:execute(
                "UPDATE division_plannings SET status = ?1, status_label = ?2, submitted_at = ?3 WHERE division_id = ?4",
                [NewStatus, StatusLabel, SubmittedAt, DivisionId]);
        _ -> {error, planning_not_found}
    end.

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
