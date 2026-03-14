-module(kanban_lifecycle_to_kanban_cards).
-behaviour(evoq_projection).

-include_lib("guide_division_lifecycle/include/kanban_card_status.hrl").

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_divisions_kanban_cards).

interested_in() ->
    [<<"kanban_card_posted_v1">>,
     <<"kanban_card_picked_v1">>,
     <<"kanban_card_finished_v1">>,
     <<"kanban_card_unpicked_v1">>,
     <<"kanban_card_parked_v1">>,
     <<"kanban_card_unparked_v1">>,
     <<"kanban_card_blocked_v1">>,
     <<"kanban_card_unblocked_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

do_project(<<"kanban_card_posted_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    CardId = gf(card_id, Data),
    Entry = #{
        division_id => DivisionId,
        card_id => CardId,
        status => ?CARD_POSTED,
        status_label => <<"posted">>,
        title => gf(title, Data),
        description => gf(description, Data),
        card_type => gf(card_type, Data),
        posted_by => gf(posted_by, Data),
        posted_at => gf(posted_at, Data)
    },
    {ok, RM2} = evoq_read_model:put({DivisionId, CardId}, Entry, RM),
    {ok, State, RM2};

do_project(<<"kanban_card_picked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_PICKED, <<"picked">>, [{picked_by, gf(picked_by, Data)}, {picked_at, gf(picked_at, Data)}], State, RM);

do_project(<<"kanban_card_finished_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_FINISHED, <<"finished">>, [{finished_at, gf(finished_at, Data)}], State, RM);

do_project(<<"kanban_card_unpicked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_POSTED, <<"posted">>, [{picked_by, undefined}, {picked_at, undefined}], State, RM);

do_project(<<"kanban_card_parked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_PARKED, <<"parked">>,
        [{parked_by, gf(parked_by, Data)}, {parked_at, gf(parked_at, Data)}, {park_reason, gf(park_reason, Data)}], State, RM);

do_project(<<"kanban_card_unparked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_POSTED, <<"posted">>, [{parked_by, undefined}, {parked_at, undefined}, {park_reason, undefined}], State, RM);

do_project(<<"kanban_card_blocked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_BLOCKED, <<"blocked">>,
        [{blocked_by, gf(blocked_by, Data)}, {blocked_at, gf(blocked_at, Data)}, {block_reason, gf(block_reason, Data)}], State, RM);

do_project(<<"kanban_card_unblocked_v1">>, Data, State, RM) ->
    update_card(Data, ?CARD_POSTED, <<"posted">>, [{blocked_by, undefined}, {blocked_at, undefined}, {block_reason, undefined}], State, RM);

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

update_card(Data, NewStatus, NewLabel, ExtraFields, State, RM) ->
    DivisionId = gf(division_id, Data),
    CardId = gf(card_id, Data),
    Key = {DivisionId, CardId},
    case evoq_read_model:get(Key, RM) of
        {ok, Card} ->
            C1 = Card#{status => NewStatus, status_label => NewLabel},
            Updated = lists:foldl(fun({K, V}, Acc) -> Acc#{K => V} end, C1, ExtraFields),
            {ok, RM2} = evoq_read_model:put(Key, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
