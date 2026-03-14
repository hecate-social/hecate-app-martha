%%% @doc Handler: maybe initiate a knowledge graph.
-module(maybe_initiate_knowledge_graph).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, dispatch/1]).

-spec handle(initiate_knowledge_graph_v1:initiate_knowledge_graph_v1()) ->
    {ok, [knowledge_graph_initiated_v1:knowledge_graph_initiated_v1()]} | {error, term()}.
handle(Cmd) ->
    case initiate_knowledge_graph_v1:validate(Cmd) of
        {ok, _} ->
            Event = knowledge_graph_initiated_v1:new(#{
                venture_id => initiate_knowledge_graph_v1:get_venture_id(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(initiate_knowledge_graph_v1:initiate_knowledge_graph_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = initiate_knowledge_graph_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = initiate_knowledge_graph,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = initiate_knowledge_graph_v1:to_map(Cmd),
        metadata = #{timestamp => erlang:system_time(millisecond),
                     aggregate_type => knowledge_graph_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    Opts = #{
        store_id => knowledge_graph_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    },
    evoq_dispatcher:dispatch(EvoqCmd, Opts).
