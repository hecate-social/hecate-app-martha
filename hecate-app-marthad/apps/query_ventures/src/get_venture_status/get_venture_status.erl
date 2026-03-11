%%% @doc Query: get venture status including division count.
-module(get_venture_status).

-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found | term()}.
get(VentureId) ->
    case project_ventures_store:get_venture(VentureId) of
        {ok, #{status := Status, status_label := StatusLabel}} ->
            DivisionCount = project_ventures_store:count_divisions(VentureId),
            {ok, #{
                venture_id => VentureId,
                status => Status,
                status_label => StatusLabel,
                discovered_divisions_count => DivisionCount
            }};
        {error, not_found} ->
            {error, not_found}
    end.
