%%% @doc Projection: storming_archived_v1 -> division_stormings table (update)
-module(storming_archived_v1_to_sqlite_division_stormings).

-include_lib("guide_division_storming/include/storming_status.hrl").

-export([project/1]).

project(Event) ->
    DivisionId = get(division_id, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, DivisionId]),
    %% First read current status
    case project_division_stormings_store:query(
        "SELECT status FROM division_stormings WHERE division_id = ?1", [DivisionId]) of
        {ok, [[CurrentStatus]]} ->
            NewStatus = CurrentStatus bor ?STORMING_ARCHIVED,
            StatusLabel = evoq_bit_flags:to_string(NewStatus, ?STORMING_FLAG_MAP),
            Sql = "UPDATE division_stormings SET status = ?1, status_label = ?2 WHERE division_id = ?3",
            project_division_stormings_store:execute(Sql, [NewStatus, StatusLabel, DivisionId]);
        {ok, []} ->
            logger:warning("[~s] storming ~s not found for archive", [?MODULE, DivisionId]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
