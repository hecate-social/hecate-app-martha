%%% @doc Handler: maybe draw a relationship between entities.
-module(maybe_draw_relationship).

-export([handle/2, dispatch/1]).

-include_lib("evoq/include/evoq.hrl").

-spec handle(draw_relationship_v1:draw_relationship_v1(), map()) ->
    {ok, [relationship_drawn_v1:relationship_drawn_v1()]} | {error, term()}.
handle(Cmd, _Context) ->
    case draw_relationship_v1:validate(Cmd) of
        {ok, _} ->
            Event = relationship_drawn_v1:new(#{
                venture_id => draw_relationship_v1:get_venture_id(Cmd),
                rel_id => draw_relationship_v1:get_rel_id(Cmd),
                from_entity => draw_relationship_v1:get_from_entity(Cmd),
                to_entity => draw_relationship_v1:get_to_entity(Cmd),
                rel_type => draw_relationship_v1:get_rel_type(Cmd),
                strength => draw_relationship_v1:get_strength(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(draw_relationship_v1:draw_relationship_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = draw_relationship_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = draw_relationship,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = draw_relationship_v1:to_map(Cmd),
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
