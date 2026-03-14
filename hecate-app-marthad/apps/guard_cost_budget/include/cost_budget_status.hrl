%%% @doc Cost budget status bit flags.
%%%
%%% Status fields in aggregates are integers treated as bit flags.
%%% Each flag is a power of 2 (unique bit position).

-define(CB_INITIATED,  1).   %% 2^0 Budget configured
-define(CB_ACTIVE,     2).   %% 2^1 Budget active and tracking
-define(CB_WARNING,    4).   %% 2^2 Spending exceeded warning threshold
-define(CB_BREACHED,   8).   %% 2^3 Spending exceeded budget

-define(CB_FLAG_MAP, #{
    1 => <<"Initiated">>,
    2 => <<"Active">>,
    4 => <<"Warning">>,
    8 => <<"Breached">>
}).
