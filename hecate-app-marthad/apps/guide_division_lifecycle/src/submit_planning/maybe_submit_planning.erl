%%% @doc maybe_submit_planning handler
%%% Business logic for submitting division planning.
-module(maybe_submit_planning).

-include_lib("evoq/include/evoq.hrl").

-export([handle/2, dispatch/1]).

-spec handle(submit_planning_v1:submit_planning_v1(), map()) ->
    {ok, [planning_submitted_v1:planning_submitted_v1()]} | {error, term()}.
handle(Cmd, Context) ->
    DivisionId = submit_planning_v1:get_division_id(Cmd),
    case validate_command(DivisionId) of
        ok ->
            Event = planning_submitted_v1:new(#{
                division_id => DivisionId,
                venture_id => maps:get(venture_id, Context),
                context_name => maps:get(context_name, Context)
            }),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(submit_planning_v1:submit_planning_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    DivisionId = submit_planning_v1:get_division_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = submit_planning,
        aggregate_type = division_aggregate,
        aggregate_id = DivisionId,
        payload = submit_planning_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => division_aggregate},
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
