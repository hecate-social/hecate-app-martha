#!/usr/bin/env bash
set -euo pipefail

## Package the Martha plugin as a .tar.gz for in-VM loading.
##
## Output: _build/plugin/hecate-app-martha.tar.gz
## Contents:
##   ebin/           - All compiled .beam + .app files (consolidated from all apps)
##   priv/static/    - Frontend assets (if built)
##   manifest.json   - Plugin metadata

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$ROOT_DIR/_build/plugin"
STAGING_DIR="$BUILD_DIR/staging"

echo "==> Compiling..."
cd "$ROOT_DIR"
rebar3 compile

echo "==> Preparing package..."
rm -rf "$STAGING_DIR"
mkdir -p "$STAGING_DIR/ebin"

## Consolidate all .beam files from root app + domain apps
## Must match the apps listed in app_martha_sup.erl and rebar.config release
DOMAIN_APPS=(
    # Martha root (sup + notation)
    martha

    # Venture lifecycle (CMD + PRJ + QRY)
    guide_venture_lifecycle
    project_ventures
    query_ventures

    # Division lifecycle (CMD + PRJ + QRY) — unified
    guide_division_lifecycle
    project_divisions
    query_divisions

    # Agent orchestration (CMD + PRJ + QRY)
    orchestrate_agents
    project_agent_sessions
    query_agent_sessions

    # Knowledge graph (CMD + PRJ + QRY)
    guide_knowledge_graph
    project_knowledge_graph
    query_knowledge_graph

    # Retry strategy (CMD + PRJ + QRY)
    guide_retry_strategy
    project_retry_strategy
    query_retry_strategy

    # Cost budget (CMD + PRJ + QRY)
    guard_cost_budget
    project_cost_budgets
    query_cost_budgets
)

for ebin_dir in \
    "$ROOT_DIR/_build/default/lib/hecate_app_marthad/ebin"; do
    if [ -d "$ebin_dir" ]; then
        cp "$ebin_dir"/*.beam "$STAGING_DIR/ebin/" 2>/dev/null || true
        cp "$ebin_dir"/*.app "$STAGING_DIR/ebin/" 2>/dev/null || true
    fi
done

for app in "${DOMAIN_APPS[@]}"; do
    ebin_dir="$ROOT_DIR/_build/default/lib/$app/ebin"
    if [ -d "$ebin_dir" ]; then
        cp "$ebin_dir"/*.beam "$STAGING_DIR/ebin/" 2>/dev/null || true
        cp "$ebin_dir"/*.app "$STAGING_DIR/ebin/" 2>/dev/null || true
    fi
done

echo "  $(find "$STAGING_DIR/ebin" -name '*.beam' | wc -l) .beam files"
echo "  $(find "$STAGING_DIR/ebin" -name '*.app' | wc -l) .app files"

## Copy static assets if they exist
STATIC_DIR="$ROOT_DIR/priv/static"
if [ -d "$STATIC_DIR" ] && [ "$(ls -A "$STATIC_DIR" 2>/dev/null)" ]; then
    mkdir -p "$STAGING_DIR/priv"
    cp -r "$STATIC_DIR" "$STAGING_DIR/priv/static"
    echo "  Static assets included"
else
    echo "  No static assets (run frontend build first)"
fi

## Copy manifest.json from repo root (single source of truth)
MANIFEST_SRC="$ROOT_DIR/../manifest.json"
if [ ! -f "$MANIFEST_SRC" ]; then
    echo "ERROR: manifest.json not found at $MANIFEST_SRC"
    exit 1
fi
cp "$MANIFEST_SRC" "$STAGING_DIR/manifest.json"

## Create the tarball
cd "$STAGING_DIR"
tar czf "$BUILD_DIR/hecate-app-martha.tar.gz" .

echo "==> Package created: $BUILD_DIR/hecate-app-martha.tar.gz"
echo "    Install with: cp $BUILD_DIR/hecate-app-martha.tar.gz ~/.hecate/plugins/martha/"
echo "    Then extract:  cd ~/.hecate/plugins/martha/ && tar xzf hecate-app-martha.tar.gz"
