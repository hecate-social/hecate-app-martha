%%% @doc Top-level supervisor for Martha ALC.
%%%
%%% Domain apps (guide_venture_lifecycle, etc.) are started by the OTP
%%% release as separate applications — NOT by this supervisor.
%%% Martha-specific web events are pushed through app_marthad_web_events
%%% by each domain's emitters — no bridge needed.
%%% @end
-module(martha_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    {ok, {SupFlags, []}}.
