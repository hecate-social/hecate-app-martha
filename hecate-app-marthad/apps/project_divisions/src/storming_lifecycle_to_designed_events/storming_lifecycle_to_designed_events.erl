-module(storming_lifecycle_to_designed_events).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_divisions_designed_events).

interested_in() -> [<<"event_designed_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    EvtName = gf(event_name, Data),
    Entry = #{
        division_id => DivisionId,
        event_name => EvtName,
        description => gf(description, Data),
        aggregate_name => gf(aggregate_name, Data),
        fields => gf(fields, Data, []),
        designed_at => gf(designed_at, Data)
    },
    {ok, RM2} = evoq_read_model:put({DivisionId, EvtName}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, Default) end.
