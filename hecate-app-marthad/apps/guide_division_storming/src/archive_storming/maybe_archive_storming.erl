%%% @doc maybe_archive_storming handler
%%% Business logic for archiving storming sessions.
-module(maybe_archive_storming).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

-spec handle(archive_storming_v1:archive_storming_v1()) ->
    {ok, [storming_archived_v1:storming_archived_v1()]} | {error, term()}.
handle(Cmd) ->
    handle(Cmd, undefined).

-spec handle(archive_storming_v1:archive_storming_v1(), term()) ->
    {ok, [storming_archived_v1:storming_archived_v1()]} | {error, term()}.
handle(Cmd, _State) ->
    DivisionId = archive_storming_v1:get_division_id(Cmd),
    case validate_command(DivisionId) of
        ok ->
            Event = storming_archived_v1:new(#{division_id => DivisionId}),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(archive_storming_v1:archive_storming_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    DivisionId = archive_storming_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = archive_storming,
        aggregate_type = division_storming_aggregate,
        aggregate_id = DivisionId,
        payload = archive_storming_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => division_storming_aggregate},
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

validate_command(DivisionId) when is_binary(DivisionId), byte_size(DivisionId) > 0 ->
    ok;
validate_command(_) ->
    {error, invalid_division_id}.
