-module(storming_lifecycle_to_planned_deps).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_divisions_planned_deps).

interested_in() -> [<<"dependency_planned_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    DepId = gf(dependency_id, Data),
    Entry = #{
        division_id => DivisionId,
        dependency_id => DepId,
        from_desk => gf(from_desk, Data),
        to_desk => gf(to_desk, Data),
        dep_type => gf(dep_type, Data),
        planned_at => gf(planned_at, Data)
    },
    {ok, RM2} = evoq_read_model:put({DivisionId, DepId}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
