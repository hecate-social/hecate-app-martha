#!/usr/bin/env bash
# Fix double-close-paren from mangled apply_event→apply conversion.
# Pattern: aggregate:apply(foo_state(), Var)),  →  aggregate:apply(foo_state(), Var),
# The extra ) was carried over from the old apply_event() call.
set -euo pipefail

DIR="$(cd "$(dirname "$0")/.." && pwd)"

FILES=(
    "$DIR/apps/guide_venture_lifecycle/test/venture_aggregate_tests.erl"
    "$DIR/apps/guide_venture_lifecycle/test/storm_aggregate_tests.erl"
    "$DIR/apps/guide_division_lifecycle/test/division_aggregate_tests.erl"
)

for f in "${FILES[@]}"; do
    if [ -f "$f" ]; then
        # Fix: _aggregate:apply(xxx_state(), Var))  →  _aggregate:apply(xxx_state(), Var)
        # Match: apply(word(), word)) → apply(word(), word)
        perl -i -pe 's/(:apply\(\w+\(\),\s*\w+)\)\)/\1\)/g' "$f"
        echo "Fixed: $f"
    fi
done

echo ""
echo "Remaining double-paren (should be none):"
grep -Pn ':apply\(\w+\(\), \w+\)\)' "$DIR/apps/"*/test/*_tests.erl 2>/dev/null || echo "(none)"
