%%% @doc Handler: maybe supersede an insight.
-module(maybe_supersede_insight).

-export([handle/2, dispatch/1]).

-include_lib("evoq/include/evoq.hrl").

-spec handle(supersede_insight_v1:supersede_insight_v1(), map()) ->
    {ok, [insight_superseded_v1:insight_superseded_v1()]} | {error, term()}.
handle(Cmd, #{insights := Insights}) ->
    InsightId = supersede_insight_v1:get_insight_id(Cmd),
    case lists:any(fun(I) -> maps:get(insight_id, I, undefined) =:= InsightId end, Insights) of
        false ->
            {error, insight_not_found};
        true ->
            case supersede_insight_v1:validate(Cmd) of
                {ok, _} ->
                    Event = insight_superseded_v1:new(#{
                        venture_id => supersede_insight_v1:get_venture_id(Cmd),
                        insight_id => InsightId,
                        superseded_by => supersede_insight_v1:get_superseded_by(Cmd),
                        reason => supersede_insight_v1:get_reason(Cmd)
                    }),
                    {ok, [Event]};
                {error, _} = Err ->
                    Err
            end
    end.

-spec dispatch(supersede_insight_v1:supersede_insight_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = supersede_insight_v1:get_venture_id(Cmd),
    EvoqCmd = #evoq_command{
        command_type = supersede_insight,
        aggregate_type = knowledge_graph_aggregate,
        aggregate_id = VentureId,
        payload = supersede_insight_v1:to_map(Cmd),
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
