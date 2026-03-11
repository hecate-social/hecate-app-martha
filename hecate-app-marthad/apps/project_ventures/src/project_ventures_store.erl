%%% @doc ETS store for project_ventures read models.
%%%
%%% Creates and owns named ETS tables for ventures, discovered_divisions,
%%% storm_sessions, event_stickies, event_stacks, event_clusters, and
%%% fact_arrows.
%%%
%%% Query functions read directly from ETS (no gen_server:call needed).
%%% @end
-module(project_ventures_store).
-behaviour(gen_server).

-include_lib("guide_venture_lifecycle/include/venture_lifecycle_status.hrl").

-export([start_link/0]).

%% Ventures
-export([get_venture/1, list_ventures/0, list_ventures_active/0]).
%% Discovered divisions
-export([list_divisions_by_venture/1, count_divisions/1]).
%% Storm sessions
-export([get_latest_storm_session/1]).
%% Event stickies
-export([list_stickies_by_storm/2]).
%% Event clusters
-export([list_clusters_by_storm/2]).
%% Fact arrows
-export([list_arrows_by_storm/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(VENTURES, project_ventures_ventures).
-define(DIVISIONS, project_ventures_divisions).
-define(STORM_SESSIONS, project_ventures_storm_sessions).
-define(STICKIES, project_ventures_stickies).
-define(STACKS, project_ventures_stacks).
-define(CLUSTERS, project_ventures_clusters).
-define(ARROWS, project_ventures_arrows).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --- Ventures ---

-spec get_venture(binary()) -> {ok, map()} | {error, not_found}.
get_venture(VentureId) ->
    case ets:lookup(?VENTURES, VentureId) of
        [{_, V}] -> {ok, V};
        [] -> {error, not_found}
    end.

-spec list_ventures() -> {ok, [map()]}.
list_ventures() ->
    All = [V || {_, V} <- ets:tab2list(?VENTURES)],
    Sorted = lists:sort(fun(A, B) ->
        maps:get(initiated_at, A, 0) >= maps:get(initiated_at, B, 0)
    end, All),
    {ok, Sorted}.

-spec list_ventures_active() -> {ok, [map()]}.
list_ventures_active() ->
    All = [V || {_, #{status := S} = V} <- ets:tab2list(?VENTURES),
                evoq_bit_flags:has_not(S, ?VL_ARCHIVED)],
    Sorted = lists:sort(fun(A, B) ->
        maps:get(initiated_at, A, 0) >= maps:get(initiated_at, B, 0)
    end, All),
    {ok, Sorted}.

%% --- Discovered Divisions ---

-spec list_divisions_by_venture(binary()) -> {ok, [map()]}.
list_divisions_by_venture(VentureId) ->
    All = [D || {_, #{venture_id := V} = D} <- ets:tab2list(?DIVISIONS),
                V =:= VentureId],
    Sorted = lists:sort(fun(A, B) ->
        maps:get(discovered_at, A, 0) >= maps:get(discovered_at, B, 0)
    end, All),
    {ok, Sorted}.

-spec count_divisions(binary()) -> non_neg_integer().
count_divisions(VentureId) ->
    length([1 || {_, #{venture_id := V}} <- ets:tab2list(?DIVISIONS),
                 V =:= VentureId]).

%% --- Storm Sessions ---

-spec get_latest_storm_session(binary()) -> {ok, map()} | {error, not_found}.
get_latest_storm_session(VentureId) ->
    Sessions = [S || {_, #{venture_id := V} = S} <- ets:tab2list(?STORM_SESSIONS),
                     V =:= VentureId],
    case Sessions of
        [] -> {error, not_found};
        _ ->
            Latest = lists:foldl(fun(S, Acc) ->
                case maps:get(storm_number, S, 0) > maps:get(storm_number, Acc, 0) of
                    true -> S;
                    false -> Acc
                end
            end, hd(Sessions), tl(Sessions)),
            {ok, Latest}
    end.

%% --- Event Stickies ---

-spec list_stickies_by_storm(binary(), non_neg_integer()) -> {ok, [map()]}.
list_stickies_by_storm(VentureId, StormNumber) ->
    All = [S || {_, #{venture_id := V, storm_number := SN} = S} <- ets:tab2list(?STICKIES),
                V =:= VentureId, SN =:= StormNumber],
    {ok, All}.

%% --- Event Clusters ---

-spec list_clusters_by_storm(binary(), non_neg_integer()) -> {ok, [map()]}.
list_clusters_by_storm(VentureId, StormNumber) ->
    All = [C || {_, #{venture_id := V, storm_number := SN} = C} <- ets:tab2list(?CLUSTERS),
                V =:= VentureId, SN =:= StormNumber],
    {ok, All}.

%% --- Fact Arrows ---

-spec list_arrows_by_storm(binary(), non_neg_integer()) -> {ok, [map()]}.
list_arrows_by_storm(VentureId, StormNumber) ->
    All = [A || {_, #{venture_id := V, storm_number := SN} = A} <- ets:tab2list(?ARROWS),
                V =:= VentureId, SN =:= StormNumber],
    {ok, All}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Tables = [
        ?VENTURES, ?DIVISIONS, ?STORM_SESSIONS,
        ?STICKIES, ?STACKS, ?CLUSTERS, ?ARROWS
    ],
    lists:foreach(fun(T) ->
        T = ets:new(T, [set, public, named_table, {read_concurrency, true}])
    end, Tables),
    {ok, #{}}.

handle_call(_Req, _From, State) -> {reply, {error, unknown_call}, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
