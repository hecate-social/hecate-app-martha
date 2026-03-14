#!/usr/bin/env bash
# Fix test assertions that expect binary event_type/command_type values.
# After evoq behaviour retrofit, to_map/1 returns atoms, not binaries.
#
# Patterns fixed:
#   ?assertEqual(<<"foo_v1">>, maps:get(event_type, ...))
#     → ?assertEqual(foo_v1, maps:get(event_type, ...))
#   ?assertEqual(<<"foo_v1">>, maps:get(command_type, ...))
#     → ?assertEqual(foo_v1, maps:get(command_type, ...))
#   lists:member(<<"foo_v1">>, EventTypes)
#     → lists:member(foo_v1, EventTypes)  (only where EventTypes comes from event_type extraction)
set -euo pipefail

DIR="$(cd "$(dirname "$0")/.." && pwd)"

find "$DIR/apps" -name "*_tests.erl" -exec \
    perl -i -pe '
        # Fix assertEqual with event_type
        s/\?assertEqual\(<<"(\w+_v\d+)">>,\s*maps\s*:\s*get\s*\(\s*event_type/?assertEqual($1, maps:get(event_type/g;
        # Fix assertEqual with command_type
        s/\?assertEqual\(<<"(\w+_v\d+)">>,\s*maps\s*:\s*get\s*\(\s*command_type/?assertEqual($1, maps:get(command_type/g;
        # Fix lists:member with binary event type in context of EventTypes
        s/lists\s*:\s*member\s*\(\s*<<\s*"(\w+_v\d+)"\s*>>/lists:member($1/g;
    ' {} +

echo "Fixed event_type/command_type assertions in test files"
echo ""
echo "Remaining binary type assertions (may need manual fix):"
grep -rn 'assertEqual(<<".*_v[0-9]">>, maps.*\(event_type\|command_type\)' "$DIR/apps/"*/test/ 2>/dev/null || echo "(none)"
