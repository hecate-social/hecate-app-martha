%%% @doc Query: get the current Big Picture Event Storming state for a venture.
%%% Assembles session, stickies, clusters, and fact arrows into one map.
-module(get_storm_state).

-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, term()}.
get(VentureId) ->
    case project_ventures_store:get_latest_storm_session(VentureId) of
        {error, not_found} ->
            {ok, #{phase => <<"ready">>, storm_number => 0,
                   stickies => [], clusters => [], arrows => []}};
        {ok, Session} ->
            StormNumber = maps:get(storm_number, Session, 0),
            {ok, Stickies} = project_ventures_store:list_stickies_by_storm(VentureId, StormNumber),
            {ok, Clusters} = project_ventures_store:list_clusters_by_storm(VentureId, StormNumber),
            {ok, Arrows} = project_ventures_store:list_arrows_by_storm(VentureId, StormNumber),
            {ok, #{
                phase => maps:get(phase, Session, <<"storm">>),
                storm_number => StormNumber,
                started_at => maps:get(started_at, Session),
                shelved_at => maps:get(shelved_at, Session),
                stickies => Stickies,
                clusters => Clusters,
                arrows => Arrows
            }}
    end.
