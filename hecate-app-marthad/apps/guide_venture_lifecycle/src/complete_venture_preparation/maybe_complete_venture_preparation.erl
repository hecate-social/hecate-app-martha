%%% @doc maybe_complete_venture_preparation handler
%%% Business logic for completing venture knowledge preparation.
%%%
%%% Guards: preparation must be active (VL_PREPARING set).
-module(maybe_complete_venture_preparation).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, dispatch/1]).

-spec handle(complete_venture_preparation_v1:complete_venture_preparation_v1()) ->
    {ok, [venture_preparation_completed_v1:venture_preparation_completed_v1()]} |
    {error, term()}.
handle(Cmd) ->
    VentureId = complete_venture_preparation_v1:get_venture_id(Cmd),
    case validate_command(VentureId) of
        ok ->
            Event = venture_preparation_completed_v1:new(#{
                venture_id => VentureId
            }),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(complete_venture_preparation_v1:complete_venture_preparation_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = complete_venture_preparation_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = complete_venture_preparation,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = complete_venture_preparation_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => venture_aggregate},
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

validate_command(VentureId) when is_binary(VentureId), byte_size(VentureId) > 0 ->
    ok;
validate_command(_) ->
    {error, invalid_venture_id}.
