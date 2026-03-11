-ifndef(STORMING_STATUS_HRL).
-define(STORMING_STATUS_HRL, true).

%% Storming dossier status
-define(STORMING_INITIATED,  1).   %% 2^0
-define(STORMING_ARCHIVED,   2).   %% 2^1
-define(STORMING_ACTIVE,     4).   %% 2^2

-define(STORMING_FLAG_MAP, #{
    ?STORMING_INITIATED  => <<"Initiated">>,
    ?STORMING_ARCHIVED   => <<"Archived">>,
    ?STORMING_ACTIVE     => <<"Active">>
}).

-endif.
