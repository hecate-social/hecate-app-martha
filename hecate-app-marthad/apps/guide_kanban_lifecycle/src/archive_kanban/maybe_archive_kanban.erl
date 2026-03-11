%%% @doc maybe_archive_kanban handler
%%% Business logic for archiving kanban boards.
-module(maybe_archive_kanban).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, dispatch/1]).

-spec handle(archive_kanban_v1:archive_kanban_v1()) ->
    {ok, [kanban_archived_v1:kanban_archived_v1()]} | {error, term()}.
handle(Cmd) ->
    DivisionId = archive_kanban_v1:get_division_id(Cmd),
    case validate_command(DivisionId) of
        ok ->
            Event = kanban_archived_v1:new(#{division_id => DivisionId}),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(archive_kanban_v1:archive_kanban_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    DivisionId = archive_kanban_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = archive_kanban,
        aggregate_type = division_kanban_aggregate,
        aggregate_id = DivisionId,
        payload = archive_kanban_v1:to_map(Cmd),
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

%% Internal
validate_command(DivisionId) when is_binary(DivisionId), byte_size(DivisionId) > 0 -> ok;
validate_command(_) -> {error, invalid_division_id}.
