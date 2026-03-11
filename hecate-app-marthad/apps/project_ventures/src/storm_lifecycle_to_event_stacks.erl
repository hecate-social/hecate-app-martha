%%% @doc Merged projection: stack_emerged_v1 -> event_stacks ETS.
-module(storm_lifecycle_to_event_stacks).
-behaviour(evoq_projection).

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_stacks).

interested_in() ->
    [<<"event_stack_emerged_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    StackId = gf(stack_id, Data),
    Stack = #{
        stack_id    => StackId,
        venture_id  => gf(venture_id, Data),
        color       => gf(color, Data),
        sticky_ids  => gf(sticky_ids, Data, []),
        status      => <<"active">>,
        emerged_at  => gf(emerged_at, Data, erlang:system_time(millisecond))
    },
    {ok, RM2} = evoq_read_model:put(StackId, Stack, RM),
    {ok, State, RM2}.

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
