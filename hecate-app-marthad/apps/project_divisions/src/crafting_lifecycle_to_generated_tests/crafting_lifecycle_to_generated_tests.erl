-module(crafting_lifecycle_to_generated_tests).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).
-define(TABLE, project_divisions_generated_tests).

interested_in() -> [<<"test_generated_v1">>].
init(_Config) -> {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}), {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    TestName = gf(test_name, Data),
    Entry = #{division_id => DivisionId, test_name => TestName, module_name => gf(module_name, Data),
              path => gf(path, Data), generated_at => gf(generated_at, Data)},
    {ok, RM2} = evoq_read_model:put({DivisionId, TestName}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
