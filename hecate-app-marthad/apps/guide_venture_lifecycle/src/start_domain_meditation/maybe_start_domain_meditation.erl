%%% @doc Handler: maybe start domain meditation.
%%% Validates at least 1 participant registered.
-module(maybe_start_domain_meditation).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

handle(Cmd) -> handle(Cmd, #{}).

handle(Cmd, Context) ->
    case start_domain_meditation_v1:validate(Cmd) of
        ok ->
            Participants = maps:get(storm_participants, Context, #{}),
            case map_size(Participants) of
                0 ->
                    {error, no_participants_registered};
                _ ->
                    Event = domain_meditation_started_v1:new(#{
                        venture_id => start_domain_meditation_v1:get_venture_id(Cmd),
                        participants => Participants
                    }),
                    {ok, [Event]}
            end;
        {error, _} = Err -> Err
    end.

dispatch(Cmd) ->
    VentureId = start_domain_meditation_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = start_domain_meditation,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = start_domain_meditation_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => venture_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
