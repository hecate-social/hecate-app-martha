-ifndef(RETRY_STATUS_HRL).
-define(RETRY_STATUS_HRL, true).

-define(RS_INITIATED, 1).   %% 2^0
-define(RS_RETRYING,  2).   %% 2^1
-define(RS_EXHAUSTED, 4).   %% 2^2
-define(RS_SUCCEEDED, 8).   %% 2^3

-define(RS_FLAG_MAP, #{
    ?RS_INITIATED => <<"Initiated">>,
    ?RS_RETRYING  => <<"Retrying">>,
    ?RS_EXHAUSTED => <<"Exhausted">>,
    ?RS_SUCCEEDED => <<"Succeeded">>
}).

-endif.
