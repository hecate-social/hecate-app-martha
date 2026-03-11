%%% @doc maybe_submit_kanban_item handler
%%% Business logic for submitting items to kanban boards.
-module(maybe_submit_kanban_item).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

-spec handle(submit_kanban_item_v1:submit_kanban_item_v1()) ->
    {ok, [kanban_item_submitted_v1:kanban_item_submitted_v1()]} | {error, term()}.
handle(Cmd) ->
    handle(Cmd, #{}).

-spec handle(submit_kanban_item_v1:submit_kanban_item_v1(), map()) ->
    {ok, [kanban_item_submitted_v1:kanban_item_submitted_v1()]} | {error, term()}.
handle(Cmd, Context) ->
    ItemId = submit_kanban_item_v1:get_item_id(Cmd),
    Items = maps:get(items, Context, #{}),
    case maps:is_key(ItemId, Items) of
        true ->
            {error, item_already_exists};
        false ->
            Event = kanban_item_submitted_v1:new(#{
                division_id => submit_kanban_item_v1:get_division_id(Cmd),
                item_id => ItemId,
                title => submit_kanban_item_v1:get_title(Cmd),
                description => submit_kanban_item_v1:get_description(Cmd),
                item_type => submit_kanban_item_v1:get_item_type(Cmd),
                submitted_by => submit_kanban_item_v1:get_submitted_by(Cmd)
            }),
            {ok, [Event]}
    end.

-spec dispatch(submit_kanban_item_v1:submit_kanban_item_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    DivisionId = submit_kanban_item_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = submit_kanban_item,
        aggregate_type = division_kanban_aggregate,
        aggregate_id = DivisionId,
        payload = submit_kanban_item_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => division_kanban_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },

    Opts = #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    },

    evoq_dispatcher:dispatch(EvoqCmd, Opts).
