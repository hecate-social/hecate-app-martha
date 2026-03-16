%%% @doc Handler: maybe contribute a meditation finding.
-module(maybe_contribute_meditation_finding).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).

handle(Cmd) -> handle(Cmd, #{}).

handle(Cmd, _Context) ->
    case contribute_meditation_finding_v1:validate(Cmd) of
        ok ->
            Event = meditation_finding_contributed_v1:new(#{
                venture_id => contribute_meditation_finding_v1:get_venture_id(Cmd),
                participant_id => contribute_meditation_finding_v1:get_participant_id(Cmd),
                finding_type => contribute_meditation_finding_v1:get_finding_type(Cmd),
                content => contribute_meditation_finding_v1:get_content(Cmd),
                sources => contribute_meditation_finding_v1:get_sources(Cmd)
            }),
            {ok, [Event]};
        {error, _} = Err -> Err
    end.

dispatch(Cmd) ->
    VentureId = contribute_meditation_finding_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),
    EvoqCmd = #evoq_command{
        command_type = contribute_meditation_finding,
        aggregate_type = venture_aggregate,
        aggregate_id = VentureId,
        payload = contribute_meditation_finding_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => venture_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => martha_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).
