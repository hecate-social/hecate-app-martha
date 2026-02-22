%%% @doc Hecate Martha top-level supervisor.
%%%
%%% Supervision tree:
%%% hecate_app_marthad_sup (one_for_one)
%%%   - app_marthad_plugin_registrar (transient worker)
%%%   - Domain app supervisors are started by their own OTP apps
%%% @end
-module(hecate_app_marthad_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        #{
            id => app_marthad_plugin_registrar,
            start => {app_marthad_plugin_registrar, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
