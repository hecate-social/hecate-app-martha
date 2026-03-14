%%% @doc Handler: maybe archive a knowledge graph.
-module(maybe_archive_knowledge_graph).

-export([handle/1, dispatch/1]).

-include_lib("evoq/include/evoq.hrl").

-spec handle(archive_knowledge_graph_v1:archive_knowledge_graph_v1()) ->
    {ok, [knowledge_graph_archived_v1:knowledge_graph_archived_v1()]} | {error, term()}.
handle(Cmd) ->
    Event = knowledge_graph_archived_v1:new(#{
        venture_id => archive_knowledge_graph_v1:get_venture_id(Cmd),
        reason => archive_knowledge_graph_v1:get_reason(Cmd)
    }),
    {ok, [Event]}.

-spec dispatch(archive_knowledge_graph_v1:archive_knowledge_graph_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = archive_knowledge_graph_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = archive_knowledge_graph,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = archive_knowledge_graph_v1:to_map(Cmd),
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
