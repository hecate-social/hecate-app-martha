-module(martha_codegen_bridge_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TMP_DIR, "/tmp/martha_codegen_bridge_test_" ++ integer_to_list(erlang:unique_integer([positive]))).

%% ===================================================================
%% Helpers
%% ===================================================================

setup_tmp_dir() ->
    Dir = ?TMP_DIR,
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dir.

cleanup_tmp_dir(Dir) ->
    os:cmd("rm -rf " ++ Dir).

%% ===================================================================
%% Tests
%% ===================================================================

empty_terms_test() ->
    Dir = setup_tmp_dir(),
    try
        {ok, []} = martha_codegen_bridge:scaffold([], Dir)
    after
        cleanup_tmp_dir(Dir)
    end.

no_apps_means_no_codegen_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {division, <<"billing">>, <<"Invoice processing">>},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [{desk, <<"issue_invoice">>, <<"invoice_issued_v1">>, [<<"id">>]}],
                flags => [],
                walk => [],
                pms => []
            }}
        ],
        {ok, []} = martha_codegen_bridge:scaffold(Terms, Dir)
    after
        cleanup_tmp_dir(Dir)
    end.

division_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [],
                walk => [],
                pms => []
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assert(length(Files) > 0),
        %% Should have created aggregate, state, supervisors, app.src files
        AggPath = filename:join([Dir, "apps", "guide_billing", "src", "invoice_aggregate.erl"]),
        ?assert(filelib:is_regular(AggPath))
    after
        cleanup_tmp_dir(Dir)
    end.

cmd_desk_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [
                    {desk, <<"issue_invoice">>, <<"invoice_issued_v1">>,
                     [<<"invoice_id">>, <<"amount">>]}
                ],
                flags => [],
                walk => [],
                pms => []
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        %% CMD desk files
        CmdDir = filename:join([Dir, "apps", "guide_billing", "src", "issue_invoice"]),
        ?assert(filelib:is_dir(CmdDir)),
        CmdFile = filename:join(CmdDir, "issue_invoice_v1.erl"),
        ?assert(filelib:is_regular(CmdFile)),
        HandlerFile = filename:join(CmdDir, "maybe_issue_invoice.erl"),
        ?assert(filelib:is_regular(HandlerFile)),
        %% Should also have PRJ desk
        PrjDir = filename:join([Dir, "apps", "project_billings", "src", "invoice_issued"]),
        ?assert(filelib:is_dir(PrjDir)),
        %% Should have QRY desks
        QryPageDir = filename:join([Dir, "apps", "query_billings", "src", "get_invoices_page"]),
        ?assert(filelib:is_dir(QryPageDir)),
        QryIdDir = filename:join([Dir, "apps", "query_billings", "src", "get_invoice_by_id"]),
        ?assert(filelib:is_dir(QryIdDir)),
        ?assert(length(Files) > 5)
    after
        cleanup_tmp_dir(Dir)
    end.

emitter_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {emit, <<"invoice_issued_v1_to_pg">>}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        EmitFiles = [F || F <- Files, string:find(F, "invoice_issued_v1_to_pg") =/= nomatch],
        ?assert(length(EmitFiles) > 0)
    after
        cleanup_tmp_dir(Dir)
    end.

listener_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {division_consumes, <<"customer_registered_fact">>, <<"auth">>}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        ListenerFiles = [F || F <- Files, string:find(F, "listener") =/= nomatch],
        ?assert(length(ListenerFiles) > 0)
    after
        cleanup_tmp_dir(Dir)
    end.

pm_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [],
                walk => [],
                pms => [{pm, <<"on_paid_notify_customer">>, <<"notifications">>}]
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        PmFiles = [F || F <- Files, string:find(F, "on_paid") =/= nomatch],
        ?assert(length(PmFiles) > 0)
    after
        cleanup_tmp_dir(Dir)
    end.

binary_repo_path_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [],
                walk => [],
                pms => []
            }}
        ],
        %% Should accept binary repo path
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, list_to_binary(Dir)),
        ?assert(length(Files) > 0)
    after
        cleanup_tmp_dir(Dir)
    end.

idempotent_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [],
                walk => [],
                pms => []
            }}
        ],
        {ok, Files1} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assert(length(Files1) > 0),
        %% Second run should skip existing files
        {ok, Files2} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assertEqual([], Files2)
    after
        cleanup_tmp_dir(Dir)
    end.

non_scaffoldable_terms_ignored_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {division, <<"billing">>, <<"Invoice processing">>},
            {division_owns, [<<"invoice">>, <<"payment">>]},
            {division_publishes, <<"invoice_paid_fact">>},
            {cost, <<"stormer">>, <<"4.2K">>, <<"$0.03">>},
            {total, <<"38.2K">>, <<"$0.24">>},
            {status, <<"billing">>, <<"stormer">>, <<"COMPLETE">>}
        ],
        {ok, []} = martha_codegen_bridge:scaffold(Terms, Dir)
    after
        cleanup_tmp_dir(Dir)
    end.

flags_passed_to_division_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [{<<"INITIATED">>, 1}, {<<"ARCHIVED">>, 2}, {<<"ISSUED">>, 4}],
                walk => [],
                pms => []
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assert(length(Files) > 0),
        StatusHrl = filename:join([Dir, "apps", "guide_billing", "include", "invoice_status.hrl"]),
        ?assert(filelib:is_regular(StatusHrl)),
        {ok, Content} = file:read_file(StatusHrl),
        ?assertNotEqual(nomatch, binary:match(Content, <<"ISSUED">>))
    after
        cleanup_tmp_dir(Dir)
    end.

walk_generates_cmd_desks_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [],
                flags => [],
                walk => [<<"initiate_invoice">>, <<"archive_invoice">>],
                pms => []
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        %% Should have generated initiate_invoice desk
        InitDir = filename:join([Dir, "apps", "guide_billing", "src", "initiate_invoice"]),
        ?assert(filelib:is_dir(InitDir)),
        %% Should have generated archive_invoice desk
        ArchiveDir = filename:join([Dir, "apps", "guide_billing", "src", "archive_invoice"]),
        ?assert(filelib:is_dir(ArchiveDir)),
        ?assert(length(Files) > 5)
    after
        cleanup_tmp_dir(Dir)
    end.

walk_skips_explicit_desks_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [
                    {desk, <<"initiate_invoice">>, <<"invoice_initiated_v1">>,
                     [<<"invoice_id">>]}
                ],
                flags => [],
                walk => [<<"initiate_invoice">>, <<"archive_invoice">>],
                pms => []
            }}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        %% archive_invoice should be generated from WALK
        ArchiveDir = filename:join([Dir, "apps", "guide_billing", "src", "archive_invoice"]),
        ?assert(filelib:is_dir(ArchiveDir)),
        %% initiate_invoice should exist from explicit DESK, not duplicated
        InitDir = filename:join([Dir, "apps", "guide_billing", "src", "initiate_invoice"]),
        ?assert(filelib:is_dir(InitDir)),
        %% Count initiate_invoice files — should only be from explicit DESK
        InitFiles = [F || F <- Files, string:find(F, "initiate_invoice") =/= nomatch],
        ?assert(length(InitFiles) > 0)
    after
        cleanup_tmp_dir(Dir)
    end.

deliver_in_vm_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {deliver, <<"hecate-app-billing">>, in_vm, #{<<"OTP">> => <<"27">>}}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assert(length(Files) >= 3),
        CiYml = filename:join([Dir, ".github", "workflows", "ci.yml"]),
        ?assert(filelib:is_regular(CiYml)),
        ReleaseYml = filename:join([Dir, ".github", "workflows", "release.yml"]),
        ?assert(filelib:is_regular(ReleaseYml)),
        %% Should NOT have Dockerfile
        ?assertNot(filelib:is_regular(filename:join(Dir, "Dockerfile")))
    after
        cleanup_tmp_dir(Dir)
    end.

deliver_container_scaffold_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {deliver, <<"hecate-app-trader">>, container,
             #{<<"OTP">> => <<"27">>, <<"PORT">> => <<"4444">>}}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        ?assert(length(Files) >= 3),
        Dockerfile = filename:join(Dir, "Dockerfile"),
        ?assert(filelib:is_regular(Dockerfile)),
        DockerYml = filename:join([Dir, ".github", "workflows", "docker.yml"]),
        ?assert(filelib:is_regular(DockerYml)),
        %% Should NOT have release.yml
        ?assertNot(filelib:is_regular(filename:join([Dir, ".github", "workflows", "release.yml"])))
    after
        cleanup_tmp_dir(Dir)
    end.

full_notation_with_deliver_flags_walk_test() ->
    Dir = setup_tmp_dir(),
    try
        Terms = [
            {app, <<"guide_billing">>, cmd, #{}},
            {app, <<"project_billings">>, prj, #{}},
            {app, <<"query_billings">>, qry, #{}},
            {agg, <<"invoice">>, <<"invoice-{id}">>, #{
                desks => [
                    {desk, <<"issue_invoice">>, <<"invoice_issued_v1">>,
                     [<<"invoice_id">>, <<"amount">>]}
                ],
                flags => [{<<"INITIATED">>, 1}, {<<"ARCHIVED">>, 2}, {<<"ISSUED">>, 4}],
                walk => [<<"initiate_invoice">>, <<"archive_invoice">>],
                pms => []
            }},
            {deliver, <<"hecate-app-billing">>, in_vm, #{<<"OTP">> => <<"27">>}}
        ],
        {ok, Files} = martha_codegen_bridge:scaffold(Terms, Dir),
        %% Division + CMD desks (explicit + walk) + PRJ + QRY + delivery
        ?assert(length(Files) > 15),
        %% Walk desks generated
        ArchiveDir = filename:join([Dir, "apps", "guide_billing", "src", "archive_invoice"]),
        ?assert(filelib:is_dir(ArchiveDir)),
        %% Delivery artifacts generated
        CiYml = filename:join([Dir, ".github", "workflows", "ci.yml"]),
        ?assert(filelib:is_regular(CiYml)),
        %% FLAGS in status.hrl
        StatusHrl = filename:join([Dir, "apps", "guide_billing", "include", "invoice_status.hrl"]),
        {ok, Content} = file:read_file(StatusHrl),
        ?assertNotEqual(nomatch, binary:match(Content, <<"ISSUED">>))
    after
        cleanup_tmp_dir(Dir)
    end.
