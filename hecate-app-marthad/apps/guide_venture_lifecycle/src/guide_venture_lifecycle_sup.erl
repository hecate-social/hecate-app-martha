%%% @doc guide_venture_lifecycle top-level supervisor
%%%
%%% Supervises all emitters for venture lifecycle events:
%%% - PG emitters: subscribe to evoq, broadcast to pg groups (internal)
%%% - Mesh emitters: subscribe to evoq, publish to mesh (external)
%%% @end
-module(guide_venture_lifecycle_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },

    Children = [
        %% ── PG emitters (internal, subscribe via evoq → broadcast to pg) ────

        %% Venture lifecycle
        emitter(venture_initiated_v1_to_pg),
        emitter(vision_refined_v1_to_pg),
        emitter(vision_submitted_v1_to_pg),
        emitter(venture_archived_v1_to_pg),

        %% Knowledge preparation
        emitter(venture_knowledge_preparation_started_v1_to_pg),

        %% Scaffold
        emitter(venture_repo_scaffolded_v1_to_pg),

        %% Discovery
        emitter(discovery_started_v1_to_pg),
        emitter(discovery_paused_v1_to_pg),
        emitter(discovery_resumed_v1_to_pg),
        emitter(discovery_completed_v1_to_pg),
        emitter(division_identified_v1_to_pg),

        %% Storm participants & meditation
        emitter(storm_participant_registered_v1_to_pg),
        emitter(storm_participant_unregistered_v1_to_pg),
        emitter(domain_meditation_started_v1_to_pg),
        emitter(meditation_finding_contributed_v1_to_pg),
        emitter(domain_meditation_completed_v1_to_pg),

        %% Meditation PM: fan out findings to knowledge graph
        emitter(on_meditation_finding_contributed_fan_out),

        %% Big Picture Storm lifecycle
        emitter(big_picture_storm_started_v1_to_pg),
        emitter(big_picture_storm_shelved_v1_to_pg),
        emitter(big_picture_storm_resumed_v1_to_pg),
        emitter(big_picture_storm_archived_v1_to_pg),
        emitter(storm_phase_advanced_v1_to_pg),

        %% Stickies
        emitter(event_sticky_posted_v1_to_pg),
        emitter(event_sticky_pulled_v1_to_pg),
        emitter(event_sticky_stacked_v1_to_pg),
        emitter(event_sticky_unstacked_v1_to_pg),
        emitter(event_sticky_clustered_v1_to_pg),
        emitter(event_sticky_unclustered_v1_to_pg),

        %% Stacks
        emitter(event_stack_emerged_v1_to_pg),
        emitter(event_stack_groomed_v1_to_pg),

        %% Clusters
        emitter(event_cluster_emerged_v1_to_pg),
        emitter(event_cluster_named_v1_to_pg),
        emitter(event_cluster_dissolved_v1_to_pg),
        emitter(event_cluster_promoted_v1_to_pg),

        %% Fact arrows
        emitter(fact_arrow_drawn_v1_to_pg),
        emitter(fact_arrow_erased_v1_to_pg),

        %% ── Mesh emitters (external, subscribe via evoq → publish to mesh) ──

        %% Venture lifecycle
        emitter(venture_initiated_v1_to_mesh),
        emitter(discovery_started_v1_to_mesh),
        emitter(division_identified_v1_to_mesh),

        %% Big Picture Storm mesh emitters
        emitter(big_picture_storm_started_v1_to_mesh),
        emitter(big_picture_storm_shelved_v1_to_mesh),
        emitter(big_picture_storm_resumed_v1_to_mesh),
        emitter(big_picture_storm_archived_v1_to_mesh),
        emitter(event_sticky_posted_v1_to_mesh),
        emitter(event_sticky_pulled_v1_to_mesh),
        emitter(event_stack_emerged_v1_to_mesh),
        emitter(event_sticky_stacked_v1_to_mesh),
        emitter(event_sticky_unstacked_v1_to_mesh),
        emitter(event_stack_groomed_v1_to_mesh),
        emitter(storm_phase_advanced_v1_to_mesh),
        emitter(event_cluster_emerged_v1_to_mesh),
        emitter(event_sticky_clustered_v1_to_mesh),
        emitter(event_sticky_unclustered_v1_to_mesh),
        emitter(event_cluster_dissolved_v1_to_mesh),
        emitter(event_cluster_named_v1_to_mesh),
        emitter(fact_arrow_drawn_v1_to_mesh),
        emitter(fact_arrow_erased_v1_to_mesh),
        emitter(event_cluster_promoted_v1_to_mesh)
    ],

    {ok, {SupFlags, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
