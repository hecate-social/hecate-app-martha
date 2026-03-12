%%% @doc Top-level supervisor for the Martha in-VM plugin.
%%%
%%% Supervises domain application supervisors.
%%% @end
-module(app_martha_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    Children = [
        %% Venture lifecycle (CMD + PRJ + QRY)
        child(guide_venture_lifecycle_sup),
        child(project_ventures_sup),
        child(query_ventures_sup),

        %% Division lifecycle (CMD + PRJ + QRY) — unified from 12 apps
        child(guide_division_lifecycle_sup),
        child(project_divisions_sup),
        child(query_divisions_sup),

        %% Agent Orchestration (CMD + PRJ + QRY)
        child(orchestrate_agents_sup),
        child(project_agent_sessions_sup),
        child(query_agent_sessions_sup),

        %% Web SSE event bridge (subscribes to $all, forwards to SSE clients)
        worker(app_marthad_event_bridge)
    ],
    {ok, {SupFlags, Children}}.

child(Mod) ->
    #{id => Mod, start => {Mod, start_link, []},
      restart => permanent, type => supervisor}.

worker(Mod) ->
    #{id => Mod, start => {Mod, start_link, []},
      restart => permanent, type => worker}.
