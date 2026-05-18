%%% @doc Shim for the moved `chat_to_llm' module.
%%%
%%% Background. Before 2026-05-18, this name resolved to a module
%%% in `hecate-social/hecate-daemon/apps/serve_llm/src/chat_to_llm/'.
%%% Martha PMs (`on_*_initiated_run_*_llm', `curate_context', …)
%%% called `chat_to_llm:chat(Model, Messages, Opts)' directly as
%%% an in-process function. The serve_llm umbrella was extracted
%%% to `hecate-services/hecate-llm' on 2026-05-18 and removed from
%%% the daemon — so those direct call sites would `undef' at
%%% runtime.
%%%
%%% This shim keeps Martha's call sites unchanged. The body
%%% translates each call into a Macula RPC against
%%% `hecate-llm.chat' (the realm-bound service that now holds the
%%% gateway logic). hecate-daemon's `hecate_mesh:call/3' resolves
%%% the procedure to whichever infrastructure node is running
%%% hecate-llm; the realm tag + macula client pool are looked up
%%% inside the daemon, not here.
%%%
%%% Migration path: when each PM is touched for other reasons,
%%% inline the `hecate_mesh:call/3' call directly + drop the
%%% chat_to_llm: prefix. When all PMs have moved, retire this
%%% module.
-module(chat_to_llm).

-export([chat/2, chat/3, chat_stream/3]).

-define(DEFAULT_TIMEOUT_MS, 60_000).
-define(PROCEDURE_CHAT,       <<"hecate-llm.chat">>).
-define(PROCEDURE_STREAM_CHAT,<<"hecate-llm.stream_chat">>).

-spec chat(binary() | atom(), list() | map()) ->
    {ok, term()} | {error, term()}.
chat(Model, Messages) ->
    chat(Model, Messages, #{}).

-spec chat(binary() | atom(), list() | map(), map()) ->
    {ok, term()} | {error, term()}.
chat(Model, Messages, Opts) ->
    Args = #{
        model    => Model,
        messages => Messages,
        opts     => Opts
    },
    Timeout = case maps:get(timeout_ms, Opts, undefined) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_TIMEOUT_MS
    end,
    case erlang:function_exported(hecate_mesh, call, 3) of
        true  -> hecate_mesh:call(?PROCEDURE_CHAT, Args, Timeout);
        false -> {error, mesh_unavailable}
    end.

%% @doc Streaming chat over the mesh.
%%
%% Today: not wired through this shim. The daemon's
%% `hecate_mesh:call/3' is unary. To stream against
%% `hecate-llm.stream_chat' (which hecate-llm DOES advertise via
%% `macula:advertise_stream/4'), use `hecate_mesh_client:call_stream'
%% directly from the calling PM and consume the returned stream.
%% Wiring it through this shim would hide the back-pressure / stream
%% lifecycle from the caller, which we don't want.
-spec chat_stream(binary() | atom(), list() | map(), map()) ->
    {error, not_implemented_via_shim}.
chat_stream(_Model, _Messages, _Opts) ->
    {error, not_implemented_via_shim}.
