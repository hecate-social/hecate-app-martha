-ifndef(KNOWLEDGE_GRAPH_STATUS_HRL).
-define(KNOWLEDGE_GRAPH_STATUS_HRL, true).

%% Bit flags for knowledge graph lifecycle status
-define(KG_INITIATED, 1).   %% 2^0
-define(KG_ACTIVE,    2).   %% 2^1
-define(KG_ARCHIVED,  4).   %% 2^2

-define(KG_FLAG_MAP, #{
    ?KG_INITIATED => <<"Initiated">>,
    ?KG_ACTIVE    => <<"Active">>,
    ?KG_ARCHIVED  => <<"Archived">>
}).

-endif.
