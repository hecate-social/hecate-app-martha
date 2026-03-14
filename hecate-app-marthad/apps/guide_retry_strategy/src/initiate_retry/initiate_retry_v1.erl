%%% @doc initiate_retry_v1 command.
%%% Initiates a retry strategy for a failed/rejected agent session.
-module(initiate_retry_v1).
-export([new/1, from_map/1, to_map/1]).
-export([get_session_id/1, get_venture_id/1, get_agent_role/1]).

-record(initiate_retry_v1, {
    session_id     :: binary(),
    venture_id     :: binary(),
    agent_role     :: binary(),
    failure_reason :: binary() | undefined,
    max_attempts   :: non_neg_integer()
}).

-opaque initiate_retry_v1() :: #initiate_retry_v1{}.
-export_type([initiate_retry_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, initiate_retry_v1()} | {error, term()}.
new(#{session_id := SId, venture_id := VId, agent_role := Role} = P) ->
    {ok, #initiate_retry_v1{
        session_id = SId,
        venture_id = VId,
        agent_role = Role,
        failure_reason = maps:get(failure_reason, P, undefined),
        max_attempts = maps:get(max_attempts, P, 3)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec to_map(initiate_retry_v1()) -> map().
to_map(#initiate_retry_v1{} = C) ->
    #{
        command_type => <<"initiate_retry">>,
        session_id => C#initiate_retry_v1.session_id,
        venture_id => C#initiate_retry_v1.venture_id,
        agent_role => C#initiate_retry_v1.agent_role,
        failure_reason => C#initiate_retry_v1.failure_reason,
        max_attempts => C#initiate_retry_v1.max_attempts
    }.

-spec from_map(map()) -> {ok, initiate_retry_v1()} | {error, term()}.
from_map(Map) ->
    SId = gv(session_id, Map),
    VId = gv(venture_id, Map),
    Role = gv(agent_role, Map),
    case {SId, VId, Role} of
        {undefined, _, _} -> {error, missing_required_fields};
        {_, undefined, _} -> {error, missing_required_fields};
        {_, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #initiate_retry_v1{
                session_id = SId,
                venture_id = VId,
                agent_role = Role,
                failure_reason = gv(failure_reason, Map),
                max_attempts = gv(max_attempts, Map, 3)
            }}
    end.

get_session_id(#initiate_retry_v1{session_id = V}) -> V.
get_venture_id(#initiate_retry_v1{venture_id = V}) -> V.
get_agent_role(#initiate_retry_v1{agent_role = V}) -> V.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
