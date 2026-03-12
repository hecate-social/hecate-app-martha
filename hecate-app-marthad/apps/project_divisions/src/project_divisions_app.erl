-module(project_divisions_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) -> project_divisions_sup:start_link().
stop(_State) -> ok.
