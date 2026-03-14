%%% @doc Cost budget aggregate state record.
%%%
%%% Tracks per-venture spending limits and accumulated costs.
%%% The warning_pct threshold triggers a warning event before full breach.

-record(cost_budget_state, {
    venture_id     = <<>>      :: binary(),
    status         = 0         :: non_neg_integer(),
    budget_usd     = 0.0       :: float(),
    spent_usd      = 0.0       :: float(),
    warning_pct    = 0.8       :: float(),
    model_policy   = #{}       :: map(),
    breached_at    = 0         :: integer(),
    initiated_at   = 0         :: integer()
}).
