-module(crafting_lifecycle_to_test_results).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).
-define(TABLE, project_divisions_test_results).

interested_in() -> [<<"test_result_recorded_v1">>].
init(_Config) -> {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}), {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    ResultId = gf(result_id, Data),
    Entry = #{division_id => DivisionId, result_id => ResultId, suite_id => gf(suite_id, Data),
              passed => gf(passed, Data), failed => gf(failed, Data), recorded_at => gf(recorded_at, Data)},
    {ok, RM2} = evoq_read_model:put({DivisionId, ResultId}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
