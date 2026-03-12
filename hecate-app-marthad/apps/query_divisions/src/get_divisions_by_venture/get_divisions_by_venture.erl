-module(get_divisions_by_venture).
-export([get/1]).

-spec get(binary()) -> {ok, [map()]}.
get(VentureId) ->
    project_divisions_store:list_divisions_by_venture(VentureId).
