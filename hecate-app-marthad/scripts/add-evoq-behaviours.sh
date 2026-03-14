#!/usr/bin/env bash
# Add evoq_command/evoq_event behaviours to all command and event modules in martha.
#
# Strategy: Any _v1.erl module with a record matching its module name is a
# command or event. We distinguish by checking to_map/1 output:
#   - Contains command_type => command module
#   - Contains event_type   => event module
#
# For each, we:
#   1. Add -behaviour(evoq_command/evoq_event) after -module()
#   2. Add -export([command_type/0]) or -export([event_type/0]) after first -export
#   3. Add command_type()/event_type() callback before new(
#   4. Fix to_map to use atom instead of binary for type value

set -euo pipefail

APPS_DIR="$(cd "$(dirname "$0")/.." && pwd)/apps"
CMD_MODIFIED=0
EVT_MODIFIED=0
CMD_SKIPPED=0
EVT_SKIPPED=0

add_command_behaviour() {
    local file="$1"
    local modname
    modname=$(grep -oP '(?<=-module\()[\w]+(?=\))' "$file")

    # Skip if already has behaviour
    if grep -q "behaviour(evoq_command)" "$file"; then
        return 1
    fi

    # 1. Add -behaviour after -module line
    sed -i "/-module($modname)\./a\\\\\\n-behaviour(evoq_command)." "$file"

    # 2. Add -export([command_type/0]) after first -export line
    local first_export_line
    first_export_line=$(grep -n "^-export" "$file" | head -1 | cut -d: -f1)
    sed -i "${first_export_line}a\\-export([command_type/0])." "$file"

    # 3. Add command_type/0 callback before new(
    local new_line
    new_line=$(grep -n "^new(" "$file" | head -1 | cut -d: -f1)
    if [ -z "$new_line" ]; then
        # Try finding -spec new
        new_line=$(grep -n "^-spec new" "$file" | head -1 | cut -d: -f1)
    fi
    if [ -n "$new_line" ]; then
        sed -i "${new_line}i\\-spec command_type() -> atom().\\ncommand_type() -> ${modname}.\\n" "$file"
    fi

    # 4. Fix to_map: command_type => <<"...">> -> command_type => modname (atom)
    # Match various patterns of binary command_type values
    sed -i "s/command_type[[:space:]]*=>[[:space:]]*<<\"[^\"]*\">>/command_type => ${modname}/g" "$file"

    CMD_MODIFIED=$((CMD_MODIFIED + 1))
    echo "  CMD ✓ $file"
    return 0
}

add_event_behaviour() {
    local file="$1"
    local modname
    modname=$(grep -oP '(?<=-module\()[\w]+(?=\))' "$file")

    # Skip if already has behaviour
    if grep -q "behaviour(evoq_event)" "$file"; then
        return 1
    fi

    # 1. Add -behaviour after -module line
    sed -i "/-module($modname)\./a\\\\\\n-behaviour(evoq_event)." "$file"

    # 2. Add -export([event_type/0]) after first -export line
    local first_export_line
    first_export_line=$(grep -n "^-export" "$file" | head -1 | cut -d: -f1)
    sed -i "${first_export_line}a\\-export([event_type/0])." "$file"

    # 3. Add event_type/0 callback before new(
    local new_line
    new_line=$(grep -n "^new(" "$file" | head -1 | cut -d: -f1)
    if [ -z "$new_line" ]; then
        new_line=$(grep -n "^-spec new" "$file" | head -1 | cut -d: -f1)
    fi
    if [ -n "$new_line" ]; then
        sed -i "${new_line}i\\-spec event_type() -> atom().\\nevent_type() -> ${modname}.\\n" "$file"
    fi

    # 4. Fix to_map: event_type => <<"...">> -> event_type => modname (atom)
    sed -i "s/event_type[[:space:]]*=>[[:space:]]*<<\"[^\"]*\">>/event_type => ${modname}/g" "$file"

    EVT_MODIFIED=$((EVT_MODIFIED + 1))
    echo "  EVT ✓ $file"
    return 0
}

echo "=== Scanning for command and event modules ==="
echo ""

while IFS= read -r file; do
    [ -z "$file" ] && continue

    modname=$(grep -oP '(?<=-module\()[\w]+(?=\))' "$file")

    # Must have a record matching module name
    if ! grep -q "^-record($modname," "$file"; then
        continue
    fi

    # Check if it's a command (has command_type in to_map) or event (has event_type in to_map)
    if grep -q "command_type[[:space:]]*=>" "$file"; then
        add_command_behaviour "$file" || CMD_SKIPPED=$((CMD_SKIPPED + 1))
    elif grep -q "event_type[[:space:]]*=>" "$file"; then
        add_event_behaviour "$file" || EVT_SKIPPED=$((EVT_SKIPPED + 1))
    fi
done < <(find "$APPS_DIR" -path "*/src/*" -name "*_v1.erl" 2>/dev/null | sort)

echo ""
echo "=== Summary ==="
echo "Commands modified: $CMD_MODIFIED"
echo "Commands skipped (already had behaviour): $CMD_SKIPPED"
echo "Events modified: $EVT_MODIFIED"
echo "Events skipped (already had behaviour): $EVT_SKIPPED"
echo "Total modified: $((CMD_MODIFIED + EVT_MODIFIED))"
