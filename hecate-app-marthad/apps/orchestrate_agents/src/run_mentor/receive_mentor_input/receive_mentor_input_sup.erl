%%% @doc Supervisor for receive_mentor_input desk.
-module(receive_mentor_input_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(mentor_input_received_v1_to_pg)

        %% PM added here:
        %% on_mentor_input_received_run_mentor_llm (resume LLM PM)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
