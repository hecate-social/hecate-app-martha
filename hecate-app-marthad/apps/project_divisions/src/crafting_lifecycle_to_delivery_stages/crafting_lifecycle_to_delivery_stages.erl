-module(crafting_lifecycle_to_delivery_stages).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).
-define(TABLE, project_divisions_delivery_stages).

interested_in() -> [<<"delivery_staged_v1">>].
init(_Config) -> {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}), {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    StageId = gf(stage_id, Data),
    Entry = #{division_id => DivisionId, stage_id => StageId, release_id => gf(release_id, Data),
              stage_name => gf(stage_name, Data), staged_at => gf(staged_at, Data)},
    {ok, RM2} = evoq_read_model:put({DivisionId, StageId}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
