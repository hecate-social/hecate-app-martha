%%% @doc query_knowledge_graph application behaviour
-module(query_knowledge_graph_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    query_knowledge_graph_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
