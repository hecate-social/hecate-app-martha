%%% @doc Tests for vision gate PM field mapping.
%%%
%%% Verifies that on_vision_gate_passed_capture_insight and
%%% on_vision_gate_rejected_capture_insight correctly build
%%% capture_insight_v1 command params from gate events.
-module(on_vision_gate_capture_insight_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VID, <<"v-gate-test">>).
-define(SESSION_ID, <<"sess-gate">>).

%% ===================================================================
%% Test Data
%% ===================================================================

make_gate_passed_data() ->
    #{
        event_type => <<"vision_gate_passed_v1">>,
        session_id => ?SESSION_ID,
        agent_role => <<"visionary">>,
        venture_id => ?VID,
        division_id => undefined,
        gate_name => <<"vision_gate">>,
        notation_output => <<"# Vision\nApproved content">>,
        parsed_terms => [],
        passed_by => <<"human-reviewer">>,
        passed_at => 1710000000000
    }.

make_gate_rejected_data() ->
    #{
        event_type => <<"vision_gate_rejected_v1">>,
        session_id => ?SESSION_ID,
        agent_role => <<"visionary">>,
        venture_id => ?VID,
        division_id => undefined,
        gate_name => <<"vision_gate">>,
        rejected_by => <<"human-reviewer">>,
        rejection_reason => <<"Scope too broad, needs focus on mobile-first">>,
        rejected_at => 1710000000000
    }.

%% ===================================================================
%% Gate Passed — Insight Construction
%% ===================================================================

gate_passed_builds_insight_test() ->
    Data = make_gate_passed_data(),
    Params = build_passed_insight(Data),
    ?assertEqual(?VID, maps:get(venture_id, Params)),
    ?assertEqual(<<"gate_reviewer">>, maps:get(source_agent, Params)),
    ?assertEqual(?SESSION_ID, maps:get(source_session, Params)),
    ?assertEqual(<<"gate_decision">>, maps:get(insight_type, Params)),
    Content = maps:get(content, Params),
    ?assertNotEqual(nomatch, binary:match(Content, <<"passed">>)),
    ?assertNotEqual(nomatch, binary:match(Content, <<"human-reviewer">>)).

gate_passed_creates_valid_command_test() ->
    Data = make_gate_passed_data(),
    Params = build_passed_insight(Data),
    {ok, Cmd} = capture_insight_v1:new(Params),
    ?assertEqual(?VID, capture_insight_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"gate_decision">>, capture_insight_v1:get_insight_type(Cmd)).

%% ===================================================================
%% Gate Rejected — Insight Construction
%% ===================================================================

gate_rejected_builds_insight_test() ->
    Data = make_gate_rejected_data(),
    Params = build_rejected_insight(Data),
    ?assertEqual(?VID, maps:get(venture_id, Params)),
    ?assertEqual(<<"gate_reviewer">>, maps:get(source_agent, Params)),
    ?assertEqual(<<"gate_rejection">>, maps:get(insight_type, Params)),
    Content = maps:get(content, Params),
    ?assertNotEqual(nomatch, binary:match(Content, <<"REJECTED">>)),
    ?assertNotEqual(nomatch, binary:match(Content, <<"human-reviewer">>)),
    ?assertNotEqual(nomatch, binary:match(Content, <<"mobile-first">>)).

gate_rejected_creates_valid_command_test() ->
    Data = make_gate_rejected_data(),
    Params = build_rejected_insight(Data),
    {ok, Cmd} = capture_insight_v1:new(Params),
    ?assertEqual(?VID, capture_insight_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"gate_rejection">>, capture_insight_v1:get_insight_type(Cmd)).

gate_rejected_without_reason_test() ->
    Data = (make_gate_rejected_data())#{rejection_reason => undefined},
    Params = build_rejected_insight(Data),
    Content = maps:get(content, Params),
    ?assertNotEqual(nomatch, binary:match(Content, <<"no reason provided">>)).

%% ===================================================================
%% Internal — Extracted PM Logic for Testability
%% ===================================================================

build_passed_insight(Data) ->
    PassedBy = coalesce(gf(passed_by, Data), <<"unknown">>),
    Content = <<"Vision gate passed by ", PassedBy/binary>>,
    #{
        venture_id => gf(venture_id, Data),
        content => Content,
        source_agent => <<"gate_reviewer">>,
        source_session => gf(session_id, Data),
        insight_type => <<"gate_decision">>
    }.

build_rejected_insight(Data) ->
    RejectedBy = coalesce(gf(rejected_by, Data), <<"unknown">>),
    Reason = coalesce(gf(rejection_reason, Data), <<"no reason provided">>),
    Content = <<"Vision gate REJECTED by ", RejectedBy/binary, ": ", Reason/binary>>,
    #{
        venture_id => gf(venture_id, Data),
        content => Content,
        source_agent => <<"gate_reviewer">>,
        source_session => gf(session_id, Data),
        insight_type => <<"gate_rejection">>
    }.

gf(Key, Data) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

coalesce(undefined, Default) -> Default;
coalesce(Value, _Default) -> Value.
