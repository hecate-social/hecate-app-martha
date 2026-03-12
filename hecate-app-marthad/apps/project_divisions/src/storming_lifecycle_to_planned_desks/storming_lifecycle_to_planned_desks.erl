-module(storming_lifecycle_to_planned_desks).
-behaviour(evoq_projection).
-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_divisions_planned_desks).

interested_in() -> [<<"desk_planned_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = _Event, _Metadata, State, RM) ->
    DivisionId = gf(division_id, Data),
    DeskName = gf(desk_name, Data),
    Entry = #{
        division_id => DivisionId,
        desk_name => DeskName,
        department => gf(department, Data),
        description => gf(description, Data),
        commands => gf(commands, Data, []),
        planned_at => gf(planned_at, Data)
    },
    {ok, RM2} = evoq_read_model:put({DivisionId, DeskName}, Entry, RM),
    {ok, State, RM2}.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, undefined) end.
gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of {ok, V} -> V; error -> maps:get(atom_to_binary(Key), Data, Default) end.
