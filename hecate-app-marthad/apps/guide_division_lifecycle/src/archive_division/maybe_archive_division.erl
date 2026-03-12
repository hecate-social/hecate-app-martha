-module(maybe_archive_division).
-include_lib("evoq/include/evoq.hrl").
-export([handle/1, handle/2, dispatch/1]).

handle(Cmd) -> handle(Cmd, undefined).

handle(Cmd, _State) ->
    DivisionId = archive_division_v1:get_division_id(Cmd),
    case is_binary(DivisionId) andalso byte_size(DivisionId) > 0 of
        true ->
            Event = division_archived_v1:new(#{division_id => DivisionId}),
            {ok, [Event]};
        false ->
            {error, invalid_division_id}
    end.

dispatch(Cmd) ->
    DivisionId = archive_division_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = archive_division,
        aggregate_type = division_aggregate,
        aggregate_id = DivisionId,
        payload = archive_division_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => division_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    Opts = #{store_id => martha_store, adapter => reckon_evoq_adapter, consistency => eventual},
    evoq_dispatcher:dispatch(EvoqCmd, Opts).
