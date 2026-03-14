%%% @doc Handler: maybe capture an insight.
-module(maybe_capture_insight).

-export([handle/1, dispatch/1]).

-include_lib("evoq/include/evoq.hrl").

-spec handle(capture_insight_v1:capture_insight_v1()) ->
    {ok, [insight_captured_v1:insight_captured_v1()]} | {error, term()}.
handle(Cmd) ->
    case capture_insight_v1:validate(Cmd) of
        {ok, _} ->
            Event = insight_captured_v1:new(#{
                venture_id => capture_insight_v1:get_venture_id(Cmd),
                insight_id => capture_insight_v1:get_insight_id(Cmd),
                content => capture_insight_v1:get_content(Cmd),
                source_agent => capture_insight_v1:get_source_agent(Cmd),
                source_session => capture_insight_v1:get_source_session(Cmd),
                insight_type => capture_insight_v1:get_insight_type(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(capture_insight_v1:capture_insight_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = capture_insight_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = capture_insight,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = capture_insight_v1:to_map(Cmd),
        metadata = #{timestamp => erlang:system_time(millisecond),
                     aggregate_type => knowledge_graph_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => knowledge_graph_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
