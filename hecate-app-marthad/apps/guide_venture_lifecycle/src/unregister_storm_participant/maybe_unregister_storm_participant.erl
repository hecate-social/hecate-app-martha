%%% @doc Handler: maybe unregister a storm participant.
-module(maybe_unregister_storm_participant).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

handle(Cmd) -> handle(Cmd, #{}).

handle(Cmd, Context) ->
    case unregister_storm_participant_v1:validate(Cmd) of
        ok ->
            ParticipantId = unregister_storm_participant_v1:get_participant_id(Cmd),
            Participants = maps:get(storm_participants, Context, #{}),
            case maps:is_key(ParticipantId, Participants) of
                false ->
                    {error, {participant_not_found, ParticipantId}};
                true ->
                    Event = storm_participant_unregistered_v1:new(#{
                        venture_id => unregister_storm_participant_v1:get_venture_id(Cmd),
                        participant_id => ParticipantId
                    }),
                    {ok, [Event]}
            end;
        {error, _} = Err -> Err
    end.

dispatch(Cmd) ->
    VentureId = unregister_storm_participant_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = unregister_storm_participant,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = unregister_storm_participant_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => venture_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
