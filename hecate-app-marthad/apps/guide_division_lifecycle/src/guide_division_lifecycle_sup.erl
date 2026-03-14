-module(guide_division_lifecycle_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
    Children = [
        %% === NEW: Division lifecycle emitters ===
        emitter(division_initiated_v1_to_pg),
        emitter(division_initiated_v1_to_mesh),
        emitter(division_archived_v1_to_pg),

        %% === Storming emitters (moved from guide_division_storming) ===
        emitter(aggregate_designed_v1_to_pg),
        emitter(event_designed_v1_to_pg),
        emitter(desk_planned_v1_to_pg),
        emitter(dependency_planned_v1_to_pg),

        %% === Planning emitters (moved from guide_division_planning) ===
        emitter(planning_opened_v1_to_pg),
        emitter(planning_shelved_v1_to_pg),
        emitter(planning_resumed_v1_to_pg),
        emitter(planning_submitted_v1_to_pg),

        %% === Kanban emitters (moved from guide_kanban_lifecycle) ===
        emitter(kanban_card_posted_v1_to_pg),
        emitter(kanban_card_picked_v1_to_pg),
        emitter(kanban_card_finished_v1_to_pg),
        emitter(kanban_card_unpicked_v1_to_pg),
        emitter(kanban_card_parked_v1_to_pg),
        emitter(kanban_card_unparked_v1_to_pg),
        emitter(kanban_card_blocked_v1_to_pg),
        emitter(kanban_card_unblocked_v1_to_pg),

        %% === Crafting emitters (moved from guide_division_crafting) ===
        emitter(crafting_opened_v1_to_pg),
        emitter(crafting_shelved_v1_to_pg),
        emitter(crafting_resumed_v1_to_pg),
        emitter(module_generated_v1_to_pg),
        emitter(test_generated_v1_to_pg),
        emitter(test_suite_run_v1_to_pg),
        emitter(test_result_recorded_v1_to_pg),
        emitter(release_delivered_v1_to_pg),
        emitter(delivery_staged_v1_to_pg),

        %% === Process Manager ===
        emitter(on_division_identified_initiate_division)
    ],
    {ok, {SupFlags, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
