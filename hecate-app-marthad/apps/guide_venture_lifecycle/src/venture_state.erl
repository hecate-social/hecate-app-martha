%%% @doc Venture state module — implements evoq_state behaviour.
%%%
%%% Owns the venture_state record, initial state creation, event folding,
%%% and serialization. Extracted from venture_aggregate to separate
%%% state concerns from command validation.
%%% @end
-module(venture_state).

-behaviour(evoq_state).

-include("venture_lifecycle_status.hrl").
-include("venture_state.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #venture_state{}.
-export_type([state/0]).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #venture_state{}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

%% Inception events
apply_event(S, #{event_type := <<"venture_initiated_v1">>} = E)      -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"vision_refined_v1">>} = E)         -> apply_vision_refined(E, S);
apply_event(S, #{event_type := <<"vision_submitted_v1">>} = E)       -> apply_vision_submitted(E, S);

%% Knowledge preparation events
apply_event(S, #{event_type := <<"venture_knowledge_preparation_started_v1">>} = E)       -> apply_preparation_started(E, S);
apply_event(S, #{event_type := <<"research_brief_contributed_v1">>} = E)                   -> apply_brief_contributed(E, S);
apply_event(S, #{event_type := <<"venture_preparation_completed_v1">>} = E)                -> apply_preparation_completed(E, S);

apply_event(S, #{event_type := <<"venture_repo_scaffolded_v1">>} = E)       -> apply_repo_scaffolded(E, S);

%% Discovery events
apply_event(S, #{event_type := <<"discovery_started_v1">>} = E)         -> apply_discovery_started(E, S);
apply_event(S, #{event_type := <<"division_identified_v1">>} = E)       -> apply_division_identified(E, S);
apply_event(S, #{event_type := <<"discovery_paused_v1">>} = E)          -> apply_discovery_paused(E, S);
apply_event(S, #{event_type := <<"discovery_resumed_v1">>} = _E)        -> apply_discovery_resumed(S);
apply_event(S, #{event_type := <<"discovery_completed_v1">>} = E)       -> apply_discovery_completed(E, S);

%% Archive
apply_event(S, #{event_type := <<"venture_archived_v1">>} = _E)       -> apply_archived(S);

%% Storm participant & meditation events
apply_event(S, #{event_type := <<"storm_participant_registered_v1">>} = E)    -> apply_participant_registered(E, S);
apply_event(S, #{event_type := <<"storm_participant_unregistered_v1">>} = E)  -> apply_participant_unregistered(E, S);
apply_event(S, #{event_type := <<"domain_meditation_started_v1">>} = E)      -> apply_meditation_started(E, S);
apply_event(S, #{event_type := <<"meditation_finding_contributed_v1">>} = E)  -> apply_finding_contributed(E, S);
apply_event(S, #{event_type := <<"domain_meditation_completed_v1">>} = E)    -> apply_meditation_completed(E, S);

%% Big Picture Event Storming events
apply_event(S, #{event_type := <<"big_picture_storm_started_v1">>} = E)        -> apply_storm_started(E, S);
apply_event(S, #{event_type := <<"event_sticky_posted_v1">>} = E)              -> apply_sticky_posted(E, S);
apply_event(S, #{event_type := <<"event_sticky_pulled_v1">>} = E)              -> apply_sticky_pulled(E, S);
apply_event(S, #{event_type := <<"event_stack_emerged_v1">>} = E)              -> apply_stack_emerged(E, S);
apply_event(S, #{event_type := <<"event_sticky_stacked_v1">>} = E)             -> apply_sticky_stacked(E, S);
apply_event(S, #{event_type := <<"event_sticky_unstacked_v1">>} = E)           -> apply_sticky_unstacked(E, S);
apply_event(S, #{event_type := <<"event_stack_groomed_v1">>} = E)              -> apply_stack_groomed(E, S);
apply_event(S, #{event_type := <<"event_cluster_emerged_v1">>} = E)            -> apply_cluster_emerged(E, S);
apply_event(S, #{event_type := <<"event_sticky_clustered_v1">>} = E)           -> apply_sticky_clustered(E, S);
apply_event(S, #{event_type := <<"event_sticky_unclustered_v1">>} = E)         -> apply_sticky_unclustered(E, S);
apply_event(S, #{event_type := <<"event_cluster_dissolved_v1">>} = E)          -> apply_cluster_dissolved(E, S);
apply_event(S, #{event_type := <<"event_cluster_named_v1">>} = E)              -> apply_cluster_named(E, S);
apply_event(S, #{event_type := <<"fact_arrow_drawn_v1">>} = E)                 -> apply_arrow_drawn(E, S);
apply_event(S, #{event_type := <<"fact_arrow_erased_v1">>} = E)                -> apply_arrow_erased(E, S);
apply_event(S, #{event_type := <<"event_cluster_promoted_v1">>} = E)           -> apply_cluster_promoted(E, S);
apply_event(S, #{event_type := <<"storm_phase_advanced_v1">>} = E)             -> apply_phase_advanced(E, S);
apply_event(S, #{event_type := <<"big_picture_storm_shelved_v1">>} = E)        -> apply_storm_shelved(E, S);
apply_event(S, #{event_type := <<"big_picture_storm_resumed_v1">>} = _E)       -> apply_storm_resumed(S);
apply_event(S, #{event_type := <<"big_picture_storm_archived_v1">>} = _E)       -> apply_storm_archived(S);

%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- to_map ---

-spec to_map(state()) -> map().
to_map(#venture_state{} = S) ->
    #{
        venture_id => S#venture_state.venture_id,
        name => S#venture_state.name,
        brief => S#venture_state.brief,
        status => S#venture_state.status,
        repo_path => S#venture_state.repo_path,
        repos => S#venture_state.repos,
        skills => S#venture_state.skills,
        context_map => S#venture_state.context_map,
        discovered_divisions => S#venture_state.discovered_divisions,
        initiated_at => S#venture_state.initiated_at,
        initiated_by => S#venture_state.initiated_by,
        discovery_started_at => S#venture_state.discovery_started_at,
        discovery_paused_at => S#venture_state.discovery_paused_at,
        discovery_completed_at => S#venture_state.discovery_completed_at,
        discovery_pause_reason => S#venture_state.discovery_pause_reason,
        research_topics => S#venture_state.research_topics,
        research_briefs => S#venture_state.research_briefs,
        preparation_started_at => S#venture_state.preparation_started_at,
        preparation_completed_at => S#venture_state.preparation_completed_at,
        storm_participants => S#venture_state.storm_participants,
        meditation_started_at => S#venture_state.meditation_started_at,
        meditation_completed_at => S#venture_state.meditation_completed_at,
        meditation_findings => S#venture_state.meditation_findings,
        storm_number => S#venture_state.storm_number,
        storm_phase => S#venture_state.storm_phase,
        storm_started_at => S#venture_state.storm_started_at,
        storm_shelved_at => S#venture_state.storm_shelved_at,
        event_stickies => S#venture_state.event_stickies,
        event_stacks => S#venture_state.event_stacks,
        event_clusters => S#venture_state.event_clusters,
        fact_arrows => S#venture_state.fact_arrows
    }.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#venture_state{
        venture_id = get_value(venture_id, E),
        name = get_value(name, E),
        brief = get_value(brief, E),
        status = evoq_bit_flags:set(0, ?VL_INITIATED),
        repos = get_value(repos, E, []),
        skills = get_value(skills, E, []),
        context_map = get_value(context_map, E, #{}),
        initiated_at = get_value(initiated_at, E),
        initiated_by = get_value(initiated_by, E)
    }.

apply_vision_refined(E, #venture_state{status = Status} = State) ->
    S1 = maybe_update(brief, E, State),
    S2 = maybe_update(repos, E, S1),
    S3 = maybe_update(skills, E, S2),
    S4 = maybe_update(context_map, E, S3),
    %% Refining the vision clears SUBMITTED — it must be re-submitted
    NewStatus = evoq_bit_flags:unset(
        evoq_bit_flags:set(Status, ?VL_VISION_REFINED),
        ?VL_SUBMITTED),
    S4#venture_state{status = NewStatus}.

apply_vision_submitted(_E, #venture_state{status = Status} = State) ->
    State#venture_state{status = evoq_bit_flags:set(Status, ?VL_SUBMITTED)}.

apply_preparation_started(E, #venture_state{status = Status} = State) ->
    Topics = get_value(research_topics, E, []),
    State#venture_state{
        status = evoq_bit_flags:set(Status, ?VL_PREPARING),
        research_topics = Topics,
        research_briefs = #{},
        preparation_started_at = get_value(started_at, E)
    }.

apply_brief_contributed(E, #venture_state{research_briefs = Briefs} = State) ->
    Topic = get_value(topic, E),
    Brief = #{
        brief => get_value(brief, E),
        agent_role => get_value(agent_role, E),
        contributed_at => get_value(contributed_at, E)
    },
    State#venture_state{research_briefs = Briefs#{Topic => Brief}}.

apply_preparation_completed(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_PREPARING),
    S1 = evoq_bit_flags:set(S0, ?VL_PREPARATION_DONE),
    State#venture_state{
        status = S1,
        preparation_completed_at = get_value(completed_at, E)
    }.

apply_repo_scaffolded(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:set(Status, ?VL_VISION_REFINED),
    S1 = evoq_bit_flags:set(S0, ?VL_SUBMITTED),
    Brief = get_value(brief, E),
    State1 = case Brief of
        undefined -> State;
        _ -> State#venture_state{brief = Brief}
    end,
    State1#venture_state{
        status = S1,
        repo_path = get_value(repo_path, E)
    }.

apply_discovery_started(E, #venture_state{status = Status} = State) ->
    State#venture_state{
        status = evoq_bit_flags:set(Status, ?VL_DISCOVERING),
        discovery_started_at = get_value(started_at, E)
    }.

apply_division_identified(E, State) ->
    DivisionId = get_value(division_id, E),
    ContextName = get_value(context_name, E),
    Discovered = State#venture_state.discovered_divisions,
    State#venture_state{
        discovered_divisions = Discovered#{ContextName => DivisionId}
    }.

apply_discovery_paused(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_DISCOVERING),
    S1 = evoq_bit_flags:set(S0, ?VL_DISCOVERY_PAUSED),
    State#venture_state{
        status = S1,
        discovery_paused_at = get_value(paused_at, E),
        discovery_pause_reason = get_value(reason, E)
    }.

apply_discovery_resumed(#venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_DISCOVERY_PAUSED),
    S1 = evoq_bit_flags:set(S0, ?VL_DISCOVERING),
    State#venture_state{
        status = S1,
        discovery_paused_at = undefined,
        discovery_pause_reason = undefined
    }.

apply_discovery_completed(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_DISCOVERING),
    S1 = evoq_bit_flags:set(S0, ?VL_DISCOVERY_COMPLETED),
    State#venture_state{
        status = S1,
        discovery_completed_at = get_value(completed_at, E)
    }.

apply_archived(#venture_state{status = Status} = State) ->
    State#venture_state{status = evoq_bit_flags:set(Status, ?VL_ARCHIVED)}.

%% --- Storm Participant & Meditation apply helpers ---

apply_participant_registered(E, #venture_state{storm_participants = Participants} = State) ->
    ParticipantId = get_value(participant_id, E),
    Participant = #{
        role => get_value(role, E),
        custom_instructions => get_value(custom_instructions, E),
        registered_at => get_value(registered_at, E)
    },
    State#venture_state{storm_participants = Participants#{ParticipantId => Participant}}.

apply_participant_unregistered(E, #venture_state{storm_participants = Participants} = State) ->
    ParticipantId = get_value(participant_id, E),
    State#venture_state{storm_participants = maps:remove(ParticipantId, Participants)}.

apply_meditation_started(E, #venture_state{status = Status} = State) ->
    State#venture_state{
        status = evoq_bit_flags:set(Status, ?VL_MEDITATING),
        meditation_started_at = get_value(started_at, E),
        meditation_findings = []
    }.

apply_finding_contributed(E, #venture_state{meditation_findings = Findings} = State) ->
    Finding = #{
        participant_id => get_value(participant_id, E),
        finding_type => get_value(finding_type, E),
        content => get_value(content, E),
        sources => get_value(sources, E, []),
        contributed_at => get_value(contributed_at, E)
    },
    State#venture_state{meditation_findings = [Finding | Findings]}.

apply_meditation_completed(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_MEDITATING),
    S1 = evoq_bit_flags:set(S0, ?VL_MEDITATION_DONE),
    State#venture_state{
        status = S1,
        meditation_completed_at = get_value(completed_at, E)
    }.

%% --- Big Picture Event Storming apply helpers ---

apply_storm_started(E, #venture_state{status = Status} = State) ->
    SN = get_value(storm_number, E, 0),
    State#venture_state{
        status = evoq_bit_flags:set(Status, ?VL_STORMING),
        storm_number = SN,
        storm_phase = storm,
        storm_started_at = get_value(started_at, E),
        storm_shelved_at = undefined,
        event_stickies = #{},
        event_stacks = #{},
        event_clusters = #{},
        fact_arrows = #{}
    }.

apply_sticky_posted(E, #venture_state{event_stickies = Stickies} = State) ->
    StickyId = get_value(sticky_id, E),
    Sticky = #{
        text => get_value(text, E),
        author => get_value(author, E, <<"user">>),
        weight => 1,
        stack_id => undefined,
        cluster_id => undefined,
        created_at => get_value(created_at, E)
    },
    State#venture_state{event_stickies = Stickies#{StickyId => Sticky}}.

apply_sticky_pulled(E, #venture_state{event_stickies = Stickies} = State) ->
    StickyId = get_value(sticky_id, E),
    State#venture_state{event_stickies = maps:remove(StickyId, Stickies)}.

apply_stack_emerged(E, #venture_state{event_stacks = Stacks} = State) ->
    StackId = get_value(stack_id, E),
    Stack = #{
        color => get_value(color, E),
        sticky_ids => []  %% stacked events populate this
    },
    State#venture_state{event_stacks = Stacks#{StackId => Stack}}.

apply_sticky_stacked(E, #venture_state{event_stickies = Stickies, event_stacks = Stacks} = State) ->
    StickyId = get_value(sticky_id, E),
    StackId = get_value(stack_id, E),
    Stickies1 = case maps:find(StickyId, Stickies) of
        {ok, S} -> Stickies#{StickyId => S#{stack_id => StackId}};
        error -> Stickies
    end,
    Stacks1 = case maps:find(StackId, Stacks) of
        {ok, Stk} ->
            Ids = maps:get(sticky_ids, Stk, []),
            Stacks#{StackId => Stk#{sticky_ids => [StickyId | Ids]}};
        error -> Stacks
    end,
    State#venture_state{event_stickies = Stickies1, event_stacks = Stacks1}.

apply_sticky_unstacked(E, #venture_state{event_stickies = Stickies, event_stacks = Stacks} = State) ->
    StickyId = get_value(sticky_id, E),
    StackId = get_value(stack_id, E),
    Stickies1 = case maps:find(StickyId, Stickies) of
        {ok, S} -> Stickies#{StickyId => S#{stack_id => undefined}};
        error -> Stickies
    end,
    Stacks1 = case maps:find(StackId, Stacks) of
        {ok, Stk} ->
            Ids = lists:delete(StickyId, maps:get(sticky_ids, Stk, [])),
            Stacks#{StackId => Stk#{sticky_ids => Ids}};
        error -> Stacks
    end,
    State#venture_state{event_stickies = Stickies1, event_stacks = Stacks1}.

apply_stack_groomed(E, #venture_state{event_stickies = Stickies, event_stacks = Stacks} = State) ->
    StackId = get_value(stack_id, E),
    CanonicalId = get_value(canonical_sticky_id, E),
    Weight = get_value(weight, E, 1),
    AbsorbedIds = get_value(absorbed_sticky_ids, E, []),
    %% Update canonical sticky weight and clear stack_id
    Stickies1 = case maps:find(CanonicalId, Stickies) of
        {ok, S} -> Stickies#{CanonicalId => S#{weight => Weight, stack_id => undefined}};
        error -> Stickies
    end,
    %% Remove absorbed stickies
    Stickies2 = lists:foldl(fun(Id, Acc) -> maps:remove(Id, Acc) end, Stickies1, AbsorbedIds),
    %% Remove the stack
    Stacks1 = maps:remove(StackId, Stacks),
    State#venture_state{event_stickies = Stickies2, event_stacks = Stacks1}.

apply_cluster_emerged(E, #venture_state{event_clusters = Clusters} = State) ->
    ClusterId = get_value(cluster_id, E),
    Cluster = #{
        name => undefined,
        color => get_value(color, E),
        sticky_ids => [],  %% clustered events populate this
        status => active
    },
    State#venture_state{event_clusters = Clusters#{ClusterId => Cluster}}.

apply_sticky_clustered(E, #venture_state{event_stickies = Stickies, event_clusters = Clusters} = State) ->
    StickyId = get_value(sticky_id, E),
    ClusterId = get_value(cluster_id, E),
    Stickies1 = case maps:find(StickyId, Stickies) of
        {ok, S} -> Stickies#{StickyId => S#{cluster_id => ClusterId}};
        error -> Stickies
    end,
    Clusters1 = case maps:find(ClusterId, Clusters) of
        {ok, C} ->
            Ids = maps:get(sticky_ids, C, []),
            Clusters#{ClusterId => C#{sticky_ids => [StickyId | Ids]}};
        error -> Clusters
    end,
    State#venture_state{event_stickies = Stickies1, event_clusters = Clusters1}.

apply_sticky_unclustered(E, #venture_state{event_stickies = Stickies, event_clusters = Clusters} = State) ->
    StickyId = get_value(sticky_id, E),
    ClusterId = get_value(cluster_id, E),
    Stickies1 = case maps:find(StickyId, Stickies) of
        {ok, S} -> Stickies#{StickyId => S#{cluster_id => undefined}};
        error -> Stickies
    end,
    Clusters1 = case maps:find(ClusterId, Clusters) of
        {ok, C} ->
            Ids = lists:delete(StickyId, maps:get(sticky_ids, C, [])),
            Clusters#{ClusterId => C#{sticky_ids => Ids}};
        error -> Clusters
    end,
    State#venture_state{event_stickies = Stickies1, event_clusters = Clusters1}.

apply_cluster_dissolved(E, #venture_state{event_stickies = Stickies, event_clusters = Clusters} = State) ->
    ClusterId = get_value(cluster_id, E),
    %% Uncluster all stickies in this cluster
    StickyIds = case maps:find(ClusterId, Clusters) of
        {ok, C} -> maps:get(sticky_ids, C, []);
        error -> []
    end,
    Stickies1 = lists:foldl(fun(Id, Acc) ->
        case maps:find(Id, Acc) of
            {ok, S} -> Acc#{Id => S#{cluster_id => undefined}};
            error -> Acc
        end
    end, Stickies, StickyIds),
    Clusters1 = case maps:find(ClusterId, Clusters) of
        {ok, C2} -> Clusters#{ClusterId => C2#{status => dissolved, sticky_ids => []}};
        error -> Clusters
    end,
    State#venture_state{event_stickies = Stickies1, event_clusters = Clusters1}.

apply_cluster_named(E, #venture_state{event_clusters = Clusters} = State) ->
    ClusterId = get_value(cluster_id, E),
    Name = get_value(name, E),
    Clusters1 = case maps:find(ClusterId, Clusters) of
        {ok, C} -> Clusters#{ClusterId => C#{name => Name}};
        error -> Clusters
    end,
    State#venture_state{event_clusters = Clusters1}.

apply_arrow_drawn(E, #venture_state{fact_arrows = Arrows} = State) ->
    ArrowId = get_value(arrow_id, E),
    Arrow = #{
        from_cluster => get_value(from_cluster, E),
        to_cluster => get_value(to_cluster, E),
        fact_name => get_value(fact_name, E)
    },
    State#venture_state{fact_arrows = Arrows#{ArrowId => Arrow}}.

apply_arrow_erased(E, #venture_state{fact_arrows = Arrows} = State) ->
    ArrowId = get_value(arrow_id, E),
    State#venture_state{fact_arrows = maps:remove(ArrowId, Arrows)}.

apply_cluster_promoted(E, #venture_state{event_clusters = Clusters} = State) ->
    ClusterId = get_value(cluster_id, E),
    Clusters1 = case maps:find(ClusterId, Clusters) of
        {ok, C} -> Clusters#{ClusterId => C#{status => promoted}};
        error -> Clusters
    end,
    State#venture_state{event_clusters = Clusters1}.

apply_phase_advanced(E, State) ->
    Phase = get_value(phase, E),
    PhaseAtom = case Phase of
        <<"storm">> -> storm;
        <<"stack">> -> stack;
        <<"groom">> -> groom;
        <<"cluster">> -> cluster;
        <<"name">> -> name;
        <<"map">> -> map;
        <<"promoted">> -> promoted;
        _ -> undefined
    end,
    State#venture_state{storm_phase = PhaseAtom}.

apply_storm_shelved(E, #venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_STORMING),
    S1 = evoq_bit_flags:set(S0, ?VL_STORM_SHELVED),
    State#venture_state{
        status = S1,
        storm_phase = shelved,
        storm_shelved_at = get_value(shelved_at, E)
    }.

apply_storm_resumed(#venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_STORM_SHELVED),
    S1 = evoq_bit_flags:set(S0, ?VL_STORMING),
    State#venture_state{
        status = S1,
        storm_phase = storm,
        storm_shelved_at = undefined
    }.

apply_storm_archived(#venture_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?VL_STORMING),
    S1 = evoq_bit_flags:unset(S0, ?VL_STORM_SHELVED),
    State#venture_state{
        status = S1,
        storm_phase = undefined,
        storm_started_at = undefined,
        storm_shelved_at = undefined,
        event_stickies = #{},
        event_stacks = #{},
        event_clusters = #{},
        fact_arrows = #{}
    }.

%% --- Internal ---

maybe_update(Field, E, State) ->
    case get_value(Field, E) of
        undefined -> State;
        Value -> set_field(Field, Value, State)
    end.

set_field(brief, V, S) -> S#venture_state{brief = V};
set_field(repos, V, S) -> S#venture_state{repos = V};
set_field(skills, V, S) -> S#venture_state{skills = V};
set_field(context_map, V, S) -> S#venture_state{context_map = V}.

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.

get_value(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
