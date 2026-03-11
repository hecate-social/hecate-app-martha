%%% @doc guide_kanban_lifecycle top-level supervisor
%%%
%%% Supervises all emitters for division kanban events:
%%% - PG emitters: subscribe to evoq, broadcast to pg groups (internal)
%%% - Process managers: cross-domain integration
%%% @end
-module(guide_kanban_lifecycle_sup).
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
        %% ── PG emitters ──

        %% Lifecycle
        #{id => kanban_initiated_v1_to_pg,
          start => {kanban_initiated_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => kanban_archived_v1_to_pg,
          start => {kanban_archived_v1_to_pg, start_link, []},
          restart => permanent, type => worker},

        %% Item operations
        #{id => kanban_item_submitted_v1_to_pg,
          start => {kanban_item_submitted_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => kanban_item_picked_v1_to_pg,
          start => {kanban_item_picked_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => kanban_item_completed_v1_to_pg,
          start => {kanban_item_completed_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => kanban_item_returned_v1_to_pg,
          start => {kanban_item_returned_v1_to_pg, start_link, []},
          restart => permanent, type => worker},

        %% ── Process managers ──

        #{id => on_division_identified_initiate_division_kanban,
          start => {on_division_identified_initiate_division_kanban, start_link, []},
          restart => permanent, type => worker}
    ],

    {ok, {SupFlags, Children}}.
