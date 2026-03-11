-ifndef(KANBAN_STATUS_HRL).
-define(KANBAN_STATUS_HRL, true).

%% Kanban board status
-define(KANBAN_INITIATED,  1).   %% 2^0
-define(KANBAN_ARCHIVED,   2).   %% 2^1
-define(KANBAN_ACTIVE,     4).   %% 2^2

-define(KANBAN_FLAG_MAP, #{
    ?KANBAN_INITIATED  => <<"Initiated">>,
    ?KANBAN_ARCHIVED   => <<"Archived">>,
    ?KANBAN_ACTIVE     => <<"Active">>
}).

-endif.
