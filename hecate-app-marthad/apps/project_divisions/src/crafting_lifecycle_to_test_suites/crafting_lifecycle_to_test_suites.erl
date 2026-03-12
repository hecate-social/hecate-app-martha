-module(crafting_lifecycle_to_test_suites).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).
-define(TABLE, project_divisions_test_suites).

interested_in() -> [<<"test_suite_run_v1">>].
init(_Config) -> {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}), {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    SuiteId = gf(suite_id, Data),
    Entry = #{division_id => DivisionId, suite_id => SuiteId, suite_name => gf(suite_name, Data), run_at => gf(run_at, Data)},
    {ok, RM2} = evoq_read_model:put({DivisionId, SuiteId}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
