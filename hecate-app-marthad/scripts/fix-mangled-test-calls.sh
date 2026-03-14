#!/usr/bin/env bash
# Fix mangled test function calls from perl regex replacement.
# Pattern: foo_state(, X) → foo_state(), X)
# This handles the case where apply_event(Event, State) was swapped
# but the perl regex ate the closing paren of a zero-arg state builder.
set -euo pipefail

DIR="$(cd "$(dirname "$0")/.." && pwd)"

FILES=(
    "$DIR/apps/guide_venture_lifecycle/test/venture_aggregate_tests.erl"
    "$DIR/apps/guide_venture_lifecycle/test/storm_aggregate_tests.erl"
    "$DIR/apps/guide_division_lifecycle/test/division_aggregate_tests.erl"
)

for f in "${FILES[@]}"; do
    if [ -f "$f" ]; then
        # Fix: _state(, → _state(),
        sed -i 's/_state(, /_state(), /g' "$f"
        echo "Fixed: $f"
    fi
done

echo ""
echo "Remaining mangled calls (should be none):"
grep -rn '_state(,' "$DIR/apps/"*/test/ 2>/dev/null || echo "(none)"
