-ifndef(RETRY_STATE_HRL).
-define(RETRY_STATE_HRL, true).

-record(retry_state, {
    session_id    = <<>>        :: binary(),
    venture_id    = <<>>        :: binary(),
    agent_role    = <<>>        :: binary(),
    status        = 0           :: non_neg_integer(),
    attempt_count = 0           :: non_neg_integer(),
    max_attempts  = 3           :: non_neg_integer(),
    adjustments   = []          :: [map()],
    last_failure  = undefined   :: binary() | undefined,
    initiated_at  = 0           :: integer(),
    last_attempt_at = 0         :: integer()
}).

-endif.
