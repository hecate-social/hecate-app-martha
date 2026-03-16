%%% @doc Handler: maybe register a storm participant.
%%% Validates role exists, enforces cap of 10 participants.
-module(maybe_register_storm_participant).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

-define(MAX_PARTICIPANTS, 10).
-define(VALID_ROLES, [
    <<"domain_expert">>, <<"boundary_spotter">>,
    <<"security_expert">>, <<"ux_expert">>
]).

handle(Cmd) -> handle(Cmd, #{}).

handle(Cmd, Context) ->
    case register_storm_participant_v1:validate(Cmd) of
        ok ->
            Role = register_storm_participant_v1:get_role(Cmd),
            Participants = maps:get(storm_participants, Context, #{}),
            case {lists:member(Role, ?VALID_ROLES), map_size(Participants) < ?MAX_PARTICIPANTS} of
                {false, _} ->
                    {error, {invalid_role, Role}};
                {_, false} ->
                    {error, max_participants_reached};
                {true, true} ->
                    Event = storm_participant_registered_v1:new(#{
                        venture_id => register_storm_participant_v1:get_venture_id(Cmd),
                        participant_id => register_storm_participant_v1:get_participant_id(Cmd),
                        role => Role,
                        custom_instructions => register_storm_participant_v1:get_custom_instructions(Cmd)
                    }),
                    {ok, [Event]}
            end;
        {error, _} = Err -> Err
    end.

dispatch(Cmd) ->
    VentureId = register_storm_participant_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = register_storm_participant,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = register_storm_participant_v1:to_map(Cmd),
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
