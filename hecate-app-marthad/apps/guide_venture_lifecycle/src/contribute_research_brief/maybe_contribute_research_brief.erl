%%% @doc maybe_contribute_research_brief handler
%%% Business logic for contributing a research brief to a venture.
%%%
%%% Guards: preparation must be active (VL_PREPARING set).
-module(maybe_contribute_research_brief).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, dispatch/1]).

-spec handle(contribute_research_brief_v1:contribute_research_brief_v1()) ->
    {ok, [research_brief_contributed_v1:research_brief_contributed_v1()]} |
    {error, term()}.
handle(Cmd) ->
    VentureId = contribute_research_brief_v1:get_venture_id(Cmd),
    Topic = contribute_research_brief_v1:get_topic(Cmd),
    Brief = contribute_research_brief_v1:get_brief(Cmd),
    Role = contribute_research_brief_v1:get_agent_role(Cmd),
    case validate_command(VentureId, Topic, Brief) of
        ok ->
            Event = research_brief_contributed_v1:new(#{
                venture_id => VentureId,
                topic => Topic,
                brief => Brief,
                agent_role => Role
            }),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

-spec dispatch(contribute_research_brief_v1:contribute_research_brief_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = contribute_research_brief_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = contribute_research_brief,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = contribute_research_brief_v1:to_map(Cmd),
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

validate_command(VentureId, Topic, Brief) when
    is_binary(VentureId), byte_size(VentureId) > 0,
    is_binary(Topic), byte_size(Topic) > 0,
    is_binary(Brief), byte_size(Brief) > 0 ->
    ok;
validate_command(_, _, _) ->
    {error, missing_required_fields}.
