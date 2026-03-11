%%% @doc Merged projection: sticky events -> event_stickies ETS.
-module(storm_lifecycle_to_event_stickies).
-behaviour(evoq_projection).

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_stickies).

interested_in() ->
    [<<"event_sticky_posted_v1">>,
     <<"event_sticky_stacked_v1">>,
     <<"event_sticky_unstacked_v1">>,
     <<"event_sticky_clustered_v1">>,
     <<"event_sticky_unclustered_v1">>,
     <<"event_stack_groomed_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

do_project(<<"event_sticky_posted_v1">>, Data, State, RM) ->
    StickyId = gf(sticky_id, Data),
    Sticky = #{
        sticky_id    => StickyId,
        venture_id   => gf(venture_id, Data),
        storm_number => gf(storm_number, Data),
        text         => gf(text, Data),
        author       => gf(author, Data, <<"user">>),
        weight       => 1,
        stack_id     => undefined,
        cluster_id   => undefined,
        created_at   => gf(created_at, Data)
    },
    {ok, RM2} = evoq_read_model:put(StickyId, Sticky, RM),
    {ok, State, RM2};

do_project(<<"event_sticky_stacked_v1">>, Data, State, RM) ->
    StickyId = gf(sticky_id, Data),
    case evoq_read_model:get(StickyId, RM) of
        {ok, S} ->
            Updated = S#{stack_id => gf(stack_id, Data)},
            {ok, RM2} = evoq_read_model:put(StickyId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_sticky_unstacked_v1">>, Data, State, RM) ->
    StickyId = gf(sticky_id, Data),
    case evoq_read_model:get(StickyId, RM) of
        {ok, S} ->
            Updated = S#{stack_id => undefined},
            {ok, RM2} = evoq_read_model:put(StickyId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_sticky_clustered_v1">>, Data, State, RM) ->
    StickyId = gf(sticky_id, Data),
    case evoq_read_model:get(StickyId, RM) of
        {ok, S} ->
            Updated = S#{cluster_id => gf(cluster_id, Data)},
            {ok, RM2} = evoq_read_model:put(StickyId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_sticky_unclustered_v1">>, Data, State, RM) ->
    StickyId = gf(sticky_id, Data),
    case evoq_read_model:get(StickyId, RM) of
        {ok, S} ->
            Updated = S#{cluster_id => undefined},
            {ok, RM2} = evoq_read_model:put(StickyId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

do_project(<<"event_stack_groomed_v1">>, Data, State, RM) ->
    CanonicalId = gf(canonical_sticky_id, Data),
    Weight = gf(weight, Data, 1),
    AbsorbedIds = gf(absorbed_sticky_ids, Data, []),
    %% Update canonical sticky
    RM2 = case evoq_read_model:get(CanonicalId, RM) of
        {ok, S} ->
            Updated = S#{weight => Weight, stack_id => undefined},
            {ok, NewRM} = evoq_read_model:put(CanonicalId, Updated, RM),
            NewRM;
        {error, not_found} ->
            RM
    end,
    %% Delete absorbed stickies
    RM3 = lists:foldl(fun(AbsId, AccRM) ->
        case evoq_read_model:delete(AbsId, AccRM) of
            {ok, DelRM} -> DelRM;
            {error, _} -> AccRM
        end
    end, RM2, AbsorbedIds),
    {ok, State, RM3};

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
