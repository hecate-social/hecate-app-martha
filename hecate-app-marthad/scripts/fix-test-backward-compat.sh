#!/usr/bin/env bash
# Fix test files that use removed backward-compat functions:
#   - {aggregate}:initial_state() → {state_mod}:new(<<>>)
#   - {aggregate}:apply_event(Event, State) → {aggregate}:apply(State, Event) [SWAPPED ARGS]
set -euo pipefail

DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Map: aggregate module → state module
declare -A STATE_MODS=(
    [venture_aggregate]=venture_state
    [division_aggregate]=division_state
    [cost_budget_aggregate]=cost_budget_state
    [knowledge_graph_aggregate]=knowledge_graph_state
    [retry_strategy_aggregate]=retry_state
    [agent_orchestration_aggregate]=agent_session_state
    [division_team_aggregate]=division_team_state
)

MODIFIED=0

for agg in "${!STATE_MODS[@]}"; do
    state_mod="${STATE_MODS[$agg]}"

    # Fix initial_state() → state_mod:new(<<>>)
    find "$DIR/apps" -name "*_tests.erl" -exec \
        sed -i "s/${agg}:initial_state()/${state_mod}:new(<<>>)/g" {} +

    # Fix apply_event(Event, State) → apply(State, Event) — SWAP the args
    # Pattern: agg:apply_event(EXPR, EXPR)
    # This is tricky with sed because expressions can be complex.
    # Use perl for better regex with balanced matching.
    find "$DIR/apps" -name "*_tests.erl" -exec \
        perl -i -pe "s/${agg}:apply_event\((\w+),\s*(\w+)\)/${agg}:apply(\$2, \$1)/g" {} +

    MODIFIED=$((MODIFIED + 1))
done

echo "Fixed references for $MODIFIED aggregate modules"
echo ""
echo "Remaining apply_event references (need manual fix if any):"
grep -rn "apply_event(" "$DIR/apps/"*/test/ 2>/dev/null || echo "(none)"
