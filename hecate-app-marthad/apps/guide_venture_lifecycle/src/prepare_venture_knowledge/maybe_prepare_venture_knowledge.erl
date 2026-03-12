%%% @doc maybe_prepare_venture_knowledge handler
%%% Business logic for starting venture knowledge preparation.
%%%
%%% Guards:
%%%   - Vision must be submitted (VL_SUBMITTED set)
%%%   - Not already preparing (VL_PREPARING not set)
%%%   - Not already prepared (VL_PREPARATION_DONE not set)
-module(maybe_prepare_venture_knowledge).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, dispatch/1]).

-spec handle(prepare_venture_knowledge_v1:prepare_venture_knowledge_v1()) ->
    {ok, [venture_knowledge_preparation_started_v1:venture_knowledge_preparation_started_v1()]} |
    {error, term()}.
handle(Cmd) ->
    VentureId = prepare_venture_knowledge_v1:get_venture_id(Cmd),
    Topics = prepare_venture_knowledge_v1:get_research_topics(Cmd),
    case validate_command(VentureId) of
        ok ->
            Event = venture_knowledge_preparation_started_v1:new(#{
                venture_id => VentureId,
                research_topics => Topics
            }),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(prepare_venture_knowledge_v1:prepare_venture_knowledge_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = prepare_venture_knowledge_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = prepare_venture_knowledge,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = prepare_venture_knowledge_v1:to_map(Cmd),
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
