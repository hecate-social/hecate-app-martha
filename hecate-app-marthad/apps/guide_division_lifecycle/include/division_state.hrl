-record(division_state, {
    division_id           :: binary() | undefined,
    venture_id            :: binary() | undefined,
    context_name          :: binary() | undefined,
    status = 0            :: non_neg_integer(),    %% division-level (initiated/archived)
    initiated_at          :: non_neg_integer() | undefined,
    initiated_by          :: binary() | undefined,

    %% Storming state
    storming_status = 0   :: non_neg_integer(),
    designed_aggregates = #{} :: map(),
    designed_events = #{}     :: map(),
    planned_desks = #{}       :: map(),
    planned_dependencies = #{} :: map(),

    %% Planning state
    planning_status = 0   :: non_neg_integer(),
    planning_opened_at    :: non_neg_integer() | undefined,
    planning_shelved_at   :: non_neg_integer() | undefined,
    planning_shelve_reason :: binary() | undefined,

    %% Kanban state
    kanban_status = 0     :: non_neg_integer(),
    cards = #{}           :: map(),

    %% Crafting state
    crafting_status = 0   :: non_neg_integer(),
    crafting_opened_at    :: non_neg_integer() | undefined,
    crafting_shelved_at   :: non_neg_integer() | undefined,
    crafting_shelve_reason :: binary() | undefined,
    generated_modules = #{} :: map(),
    generated_tests = #{}   :: map(),
    test_suites = #{}       :: map(),
    test_results = #{}      :: map(),
    releases = #{}          :: map(),
    delivery_stages = #{}   :: map()
}).
