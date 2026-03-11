%%% @doc Query: get a venture by its ID.
-module(get_venture_by_id).

-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found | term()}.
get(VentureId) ->
    project_ventures_store:get_venture(VentureId).
