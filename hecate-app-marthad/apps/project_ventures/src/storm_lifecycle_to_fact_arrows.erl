%%% @doc Merged projection: arrow events -> fact_arrows ETS.
-module(storm_lifecycle_to_fact_arrows).
-behaviour(evoq_projection).

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_arrows).

interested_in() ->
    [<<"fact_arrow_drawn_v1">>,
     <<"fact_arrow_erased_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

do_project(<<"fact_arrow_drawn_v1">>, Data, State, RM) ->
    ArrowId = gf(arrow_id, Data),
    Arrow = #{
        arrow_id     => ArrowId,
        venture_id   => gf(venture_id, Data),
        storm_number => gf(storm_number, Data, 0),
        from_cluster => gf(from_cluster, Data),
        to_cluster   => gf(to_cluster, Data),
        fact_name    => gf(fact_name, Data),
        created_at   => gf(drawn_at, Data, erlang:system_time(millisecond))
    },
    {ok, RM2} = evoq_read_model:put(ArrowId, Arrow, RM),
    {ok, State, RM2};

do_project(<<"fact_arrow_erased_v1">>, Data, State, RM) ->
    ArrowId = gf(arrow_id, Data),
    case evoq_read_model:delete(ArrowId, RM) of
        {ok, RM2} -> {ok, State, RM2};
        {error, _} -> {skip, State, RM}
    end;

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(#{<<"event_type">> := T}) when is_binary(T) -> T;
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
