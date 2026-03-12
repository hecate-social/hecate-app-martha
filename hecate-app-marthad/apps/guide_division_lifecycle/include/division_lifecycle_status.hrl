%% Division-level flags
-define(DIV_INITIATED,  1).    %% 2^0
-define(DIV_ARCHIVED,   2).    %% 2^1

-define(DIV_FLAG_MAP, #{
    1 => <<"initiated">>,
    2 => <<"archived">>
}).

%% Storming phase flags (separate integer field)
-define(STORMING_INITIATED, 1).
-define(STORMING_ARCHIVED,  2).
-define(STORMING_ACTIVE,    4).

-define(STORMING_FLAG_MAP, #{
    1 => <<"initiated">>,
    2 => <<"archived">>,
    4 => <<"active">>
}).

%% Planning phase flags (separate integer field)
-define(PLANNING_INITIATED,  1).
-define(PLANNING_ARCHIVED,   2).
-define(PLANNING_OPEN,       4).
-define(PLANNING_SHELVED,    8).
-define(PLANNING_SUBMITTED, 16).

-define(PLANNING_FLAG_MAP, #{
    1 => <<"initiated">>,
    2 => <<"archived">>,
    4 => <<"open">>,
    8 => <<"shelved">>,
    16 => <<"submitted">>
}).

%% Kanban board flags (separate integer field)
-define(BOARD_INITIATED, 1).
-define(BOARD_ARCHIVED,  2).
-define(BOARD_ACTIVE,    4).

-define(BOARD_FLAG_MAP, #{
    1 => <<"initiated">>,
    2 => <<"archived">>,
    4 => <<"active">>
}).

%% Crafting phase flags (separate integer field)
-define(CRAFTING_INITIATED, 1).
-define(CRAFTING_ARCHIVED,  2).
-define(CRAFTING_OPEN,      4).
-define(CRAFTING_SHELVED,   8).

-define(CRAFTING_FLAG_MAP, #{
    1 => <<"initiated">>,
    2 => <<"archived">>,
    4 => <<"open">>,
    8 => <<"shelved">>
}).
