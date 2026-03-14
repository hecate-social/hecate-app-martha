%%% @doc Merged projection: cluster events -> event_clusters ETS.
-module(storm_lifecycle_to_event_clusters).
-behaviour(evoq_projection).

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_clusters).

interested_in() ->
    [<<"event_cluster_emerged_v1">>,
     <<"event_cluster_named_v1">>,
     <<"event_cluster_promoted_v1">>,
     <<"event_cluster_dissolved_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

do_project(<<"event_cluster_emerged_v1">>, Data, State, RM) ->
    ClusterId = gf(cluster_id, Data),
    Cluster = #{
        cluster_id   => ClusterId,
        venture_id   => gf(venture_id, Data),
        storm_number => gf(storm_number, Data, 0),
        name         => undefined,
        color        => gf(color, Data),
        status       => <<"active">>,
        created_at   => gf(emerged_at, Data, erlang:system_time(millisecond))
    },
    {ok, RM2} = evoq_read_model:put(ClusterId, Cluster, RM),
    {ok, State, RM2};

do_project(<<"event_cluster_named_v1">>, Data, State, RM) ->
    ClusterId = gf(cluster_id, Data),
    case evoq_read_model:get(ClusterId, RM) of
        {ok, C} ->
            Updated = C#{name => gf(name, Data)},
            {ok, RM2} = evoq_read_model:put(ClusterId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_cluster_promoted_v1">>, Data, State, RM) ->
    ClusterId = gf(cluster_id, Data),
    case evoq_read_model:get(ClusterId, RM) of
        {ok, C} ->
            Updated = C#{status => <<"promoted">>},
            {ok, RM2} = evoq_read_model:put(ClusterId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_cluster_dissolved_v1">>, Data, State, RM) ->
    ClusterId = gf(cluster_id, Data),
    case evoq_read_model:get(ClusterId, RM) of
        {ok, C} ->
            Updated = C#{status => <<"dissolved">>},
            {ok, RM2} = evoq_read_model:put(ClusterId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, Default)
    end.
