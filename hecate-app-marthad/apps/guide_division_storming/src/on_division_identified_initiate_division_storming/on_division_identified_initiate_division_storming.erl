%%% @doc Process manager: on division_identified_v1, initiate division storming.
%%% Subscribes to division_identified_v1 from martha_store and dispatches
%%% initiate_storming_v1 command to create a storming session per division.
-module(on_division_identified_initiate_division_storming).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(EVENT_TYPE, <<"division_identified_v1">>).
-define(SUB_NAME, <<"on_division_identified_initiate_division_storming">>).
-define(STORE_ID, martha_store).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        ?STORE_ID, event_type, ?EVENT_TYPE, ?SUB_NAME,
        #{subscriber_pid => self()}),
    {ok, #{}}.

handle_info({events, Events}, State) ->
    lists:foreach(fun process_event/1, Events),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%% Internal

process_event(RawEvent) ->
    Event = app_marthad_projection_event:to_map(RawEvent),
    DivisionId = get_value(division_id, Event),
    VentureId = get_value(venture_id, Event),
    ContextName = get_value(context_name, Event),
    case {DivisionId, VentureId, ContextName} of
        {undefined, _, _} ->
            logger:warning("[~s] missing division_id in event", [?MODULE]);
        {_, undefined, _} ->
            logger:warning("[~s] missing venture_id in event", [?MODULE]);
        {_, _, undefined} ->
            logger:warning("[~s] missing context_name in event", [?MODULE]);
        _ ->
            CmdParams = #{
                division_id => DivisionId,
                venture_id => VentureId,
                context_name => ContextName,
                initiated_by => <<"system:pm">>
            },
            case initiate_storming_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_initiate_storming:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] initiated storming for ~s", [?MODULE, DivisionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to initiate storming for ~s: ~p",
                                          [?MODULE, DivisionId, Reason])
                    end;
                {error, Reason} ->
                    logger:warning("[~s] failed to create command: ~p", [?MODULE, Reason])
            end
    end.

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
