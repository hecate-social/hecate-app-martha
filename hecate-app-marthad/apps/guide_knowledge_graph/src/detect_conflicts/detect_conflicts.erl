%%% @doc Cross-division conflict detector.
%%% Stateless desk in guide_knowledge_graph. Compares entities across
%%% divisions within the same venture to detect type collisions.
%%% E.g., Division A uses PostgreSQL while Division B uses SQLite for "database".
-module(detect_conflicts).
-export([detect/2]).

%% @doc Detect conflicts between a new entity and existing entities across divisions.
%% Returns list of conflict warning binaries.
-spec detect(map(), map()) -> [binary()].
detect(NewEntity, Graph) ->
    NewName = gv(<<"name">>, NewEntity, <<>>),
    NewType = gv(<<"type">>, NewEntity, <<>>),
    NewDivision = gv(<<"division_id">>, NewEntity, undefined),
    ExistingEntities = get_entity_list(Graph),
    Conflicts = [build_warning(NewEntity, Existing)
                 || Existing <- ExistingEntities,
                    is_conflict(NewName, NewType, NewDivision, Existing)],
    Conflicts.

%% --- Internal ---

%% Two entities conflict if:
%% 1. Same type (e.g., both "database")
%% 2. Different names (e.g., "PostgreSQL" vs "SQLite")
%% 3. Different divisions (cross-division only)
is_conflict(NewName, NewType, NewDivision, Existing) ->
    ExName = gv(<<"name">>, Existing, <<>>),
    ExType = gv(<<"type">>, Existing, <<>>),
    ExDivision = gv(<<"division_id">>, Existing, undefined),
    NewType =:= ExType andalso
    NewType =/= <<>> andalso
    NewName =/= ExName andalso
    NewName =/= <<>> andalso
    ExName =/= <<>> andalso
    NewDivision =/= ExDivision andalso
    NewDivision =/= undefined andalso
    ExDivision =/= undefined.

build_warning(NewEntity, Existing) ->
    NewName = gv(<<"name">>, NewEntity, <<>>),
    NewType = gv(<<"type">>, NewEntity, <<>>),
    NewDiv = gv(<<"division_id">>, NewEntity, <<"unknown">>),
    ExName = gv(<<"name">>, Existing, <<>>),
    ExDiv = gv(<<"division_id">>, Existing, <<"unknown">>),
    iolist_to_binary([
        <<"Cross-division conflict: ">>,
        NewName, <<" (">>, NewType, <<") in division ">>, NewDiv,
        <<" vs ">>,
        ExName, <<" (">>, NewType, <<") in division ">>, ExDiv,
        <<". Both serve the same role — resolve or justify.">>
    ]).

get_entity_list(#{entities := Entities}) when is_map(Entities) ->
    maps:values(Entities);
get_entity_list(#{<<"entities">> := Entities}) when is_map(Entities) ->
    maps:values(Entities);
get_entity_list(_) ->
    [].

gv(Key, Map, Default) when is_binary(Key) ->
    maps:get(Key, Map, maps:get(binary_to_atom(Key), Map, Default)).
