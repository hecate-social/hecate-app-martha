%%% @doc maybe_return_kanban_item handler
-module(maybe_return_kanban_item).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

-spec handle(return_kanban_item_v1:return_kanban_item_v1()) ->
    {ok, [kanban_item_returned_v1:kanban_item_returned_v1()]} | {error, term()}.
handle(Cmd) -> handle(Cmd, #{}).

-spec handle(return_kanban_item_v1:return_kanban_item_v1(), map()) ->
    {ok, [kanban_item_returned_v1:kanban_item_returned_v1()]} | {error, term()}.
handle(Cmd, Context) ->
    ItemId = return_kanban_item_v1:get_item_id(Cmd),
    Items = maps:get(items, Context, #{}),
    case maps:find(ItemId, Items) of
        {ok, #{status := in_progress}} ->
            Event = kanban_item_returned_v1:new(#{
                division_id => return_kanban_item_v1:get_division_id(Cmd),
                item_id => ItemId,
                reason => return_kanban_item_v1:get_reason(Cmd)
            }),
            {ok, [Event]};
        {ok, _} ->
            {error, item_not_in_progress};
        error ->
            {error, item_not_found}
    end.

-spec dispatch(return_kanban_item_v1:return_kanban_item_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    DivisionId = return_kanban_item_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = return_kanban_item,
        aggregate_type = division_kanban_aggregate,
        aggregate_id = DivisionId,
        payload = return_kanban_item_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => division_kanban_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
