%%% @doc Handler: maybe recognize an entity.
-module(maybe_recognize_entity).

-export([handle/2, dispatch/1]).

-include_lib("evoq/include/evoq.hrl").

-spec handle(recognize_entity_v1:recognize_entity_v1(), map()) ->
    {ok, [entity_recognized_v1:entity_recognized_v1()]} | {error, term()}.
handle(Cmd, _Context) ->
    case recognize_entity_v1:validate(Cmd) of
        {ok, _} ->
            Event = entity_recognized_v1:new(#{
                venture_id => recognize_entity_v1:get_venture_id(Cmd),
                entity_id => recognize_entity_v1:get_entity_id(Cmd),
                entity_type => recognize_entity_v1:get_entity_type(Cmd),
                name => recognize_entity_v1:get_name(Cmd),
                description => recognize_entity_v1:get_description(Cmd),
                source_agent => recognize_entity_v1:get_source_agent(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(recognize_entity_v1:recognize_entity_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = recognize_entity_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = recognize_entity,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = recognize_entity_v1:to_map(Cmd),
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
