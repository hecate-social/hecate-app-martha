%%% @doc Handler: maybe complete domain meditation.
-module(maybe_complete_domain_meditation).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

handle(Cmd) -> handle(Cmd, #{}).

handle(Cmd, _Context) ->
    case complete_domain_meditation_v1:validate(Cmd) of
        ok ->
            Event = domain_meditation_completed_v1:new(#{
                venture_id => complete_domain_meditation_v1:get_venture_id(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err -> Err
    end.

dispatch(Cmd) ->
    VentureId = complete_domain_meditation_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = complete_domain_meditation,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = complete_domain_meditation_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => venture_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
