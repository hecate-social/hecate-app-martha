#!/usr/bin/env bash
# Extract all on_* PM modules from nested desk directories
# to their own on_* directories at src/ level.
#
# This is a purely mechanical refactoring — rebar3 compiles
# all .erl files under src/ recursively regardless of nesting.

set -euo pipefail

SRC="apps/orchestrate_agents/src"

# Find all on_*.erl files, extract each to its own on_* directory
find "$SRC" -name "on_*.erl" -type f | while read -r pm_file; do
    module=$(basename "$pm_file" .erl)
    target_dir="$SRC/$module"

    # Skip if already at the right level
    current_dir=$(dirname "$pm_file")
    if [ "$current_dir" = "$target_dir" ]; then
        echo "SKIP (already correct): $module"
        continue
    fi

    mkdir -p "$target_dir"
    mv "$pm_file" "$target_dir/"
    echo "MOVED: $pm_file -> $target_dir/$module.erl"
done

echo ""
echo "=== on_* directories at src/ level ==="
ls -d "$SRC"/on_* 2>/dev/null || echo "(none)"
