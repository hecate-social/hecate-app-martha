-define(CARD_POSTED,   1).
-define(CARD_PICKED,   2).
-define(CARD_FINISHED, 4).
-define(CARD_PARKED,   8).
-define(CARD_BLOCKED, 16).
-define(CARD_BACKLOG, 32).

-define(CARD_FLAG_MAP, #{
    1  => <<"posted">>,
    2  => <<"picked">>,
    4  => <<"finished">>,
    8  => <<"parked">>,
    16 => <<"blocked">>,
    32 => <<"backlog">>
}).
