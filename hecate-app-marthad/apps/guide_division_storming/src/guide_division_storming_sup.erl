%%% @doc guide_division_storming top-level supervisor
%%%
%%% Supervises all emitters for division storming events:
%%% - PG emitters: subscribe to evoq, broadcast to pg groups (internal)
%%% - Process managers: cross-domain integration
%%% @end
-module(guide_division_storming_sup).
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
        %% ── PG emitters (internal, subscribe via evoq -> broadcast to pg) ────

        %% Storming lifecycle
        #{id => storming_initiated_v1_to_pg,
          start => {storming_initiated_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => storming_archived_v1_to_pg,
          start => {storming_archived_v1_to_pg, start_link, []},
          restart => permanent, type => worker},

        %% Design + plan desks
        #{id => aggregate_designed_v1_to_pg,
          start => {aggregate_designed_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => event_designed_v1_to_pg,
          start => {event_designed_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => desk_planned_v1_to_pg,
          start => {desk_planned_v1_to_pg, start_link, []},
          restart => permanent, type => worker},
        #{id => dependency_planned_v1_to_pg,
          start => {dependency_planned_v1_to_pg, start_link, []},
          restart => permanent, type => worker},

        %% ── Process managers ──

        #{id => on_division_identified_initiate_division_storming,
          start => {on_division_identified_initiate_division_storming, start_link, []},
          restart => permanent, type => worker}
    ],

    {ok, {SupFlags, Children}}.
