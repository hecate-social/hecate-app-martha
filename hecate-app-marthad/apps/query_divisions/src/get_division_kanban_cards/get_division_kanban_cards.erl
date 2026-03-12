-module(get_division_kanban_cards).
-compile({no_auto_import,[get/1]}).
-export([get/1, get/2]).

-include_lib("guide_division_lifecycle/include/kanban_card_status.hrl").

-spec get(binary()) -> {ok, [map()]}.
get(DivisionId) ->
    project_divisions_store:list_kanban_cards(DivisionId).

-spec get(binary(), map()) -> {ok, [map()]}.
get(DivisionId, #{status := StatusBin}) ->
    StatusInt = status_to_int(StatusBin),
    project_divisions_store:list_kanban_cards_by_status(DivisionId, StatusInt);
get(DivisionId, _) ->
    get(DivisionId).

status_to_int(<<"posted">>)   -> ?CARD_POSTED;
status_to_int(<<"picked">>)   -> ?CARD_PICKED;
status_to_int(<<"finished">>) -> ?CARD_FINISHED;
status_to_int(<<"parked">>)   -> ?CARD_PARKED;
status_to_int(<<"blocked">>)  -> ?CARD_BLOCKED;
status_to_int(<<"backlog">>)  -> ?CARD_BACKLOG;
status_to_int(_)              -> 0.
