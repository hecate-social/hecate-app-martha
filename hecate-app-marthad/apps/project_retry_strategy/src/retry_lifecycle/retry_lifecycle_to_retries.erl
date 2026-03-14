%%% @doc Merged projection: all retry lifecycle events to ETS retries table.
%%% Single projection handles all event types to prevent ETS races.
-module(retry_lifecycle_to_retries).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_retry_strategy).

interested_in() ->
    [<<"retry_initiated_v1">>,
     <<"retry_attempted_v1">>,
     <<"retry_exhausted_v1">>,
     <<"retry_succeeded_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(Event, _Metadata, State, RM) ->
    Data = maps:get(data, Event, Event),
    EventType = gv(event_type, Data),
    do_project(EventType, Data, State, RM).

%% --- Event handlers ---

do_project(<<"retry_initiated_v1">>, Data, State, RM) ->
    SessionId = gv(<<"session_id">>, Data),
    Record = #{
        <<"session_id">> => SessionId,
        <<"venture_id">> => gv(<<"venture_id">>, Data),
        <<"agent_role">> => gv(<<"agent_role">>, Data),
        <<"status">> => <<"initiated">>,
        <<"attempt_count">> => 0,
        <<"max_attempts">> => gv(<<"max_attempts">>, Data, 3),
        <<"failure_reason">> => gv(<<"failure_reason">>, Data),
        <<"adjustments">> => [],
        <<"initiated_at">> => gv(<<"initiated_at">>, Data)
    },
    {ok, RM2} = evoq_read_model:put(SessionId, Record, RM),
    {ok, State, RM2};

do_project(<<"retry_attempted_v1">>, Data, State, RM) ->
    SessionId = gv(<<"session_id">>, Data),
    update_retry(SessionId, State, RM, fun(R) ->
        Adjs = maps:get(<<"adjustments">>, R, []),
        R#{
            <<"status">> => <<"retrying">>,
            <<"attempt_count">> => gv(<<"attempt_number">>, Data, 0),
            <<"adjustments">> => Adjs ++ [gv(<<"adjustment">>, Data, #{})],
            <<"last_attempt_at">> => gv(<<"attempted_at">>, Data)
        }
    end);

do_project(<<"retry_exhausted_v1">>, Data, State, RM) ->
    SessionId = gv(<<"session_id">>, Data),
    update_retry(SessionId, State, RM, fun(R) ->
        R#{<<"status">> => <<"exhausted">>,
           <<"exhausted_at">> => gv(<<"exhausted_at">>, Data)}
    end);

do_project(<<"retry_succeeded_v1">>, Data, State, RM) ->
    SessionId = gv(<<"session_id">>, Data),
    update_retry(SessionId, State, RM, fun(R) ->
        R#{<<"status">> => <<"succeeded">>,
           <<"succeeded_at">> => gv(<<"succeeded_at">>, Data)}
    end);

do_project(_EventType, _Data, State, RM) ->
    {ok, State, RM}.

%% --- Helpers ---

update_retry(SessionId, State, RM, UpdateFn) ->
    case evoq_read_model:get(SessionId, RM) of
        {ok, Existing} ->
            Updated = UpdateFn(Existing),
            {ok, RM2} = evoq_read_model:put(SessionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_binary(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(binary_to_atom(Key), Map) of
                {ok, V2} -> V2;
                error -> Default
            end
    end.
