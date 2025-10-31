#!/bin/bash
FILE="$1"

# Get the current commit hash being edited
COMMIT_HASH=$(grep -E "^# This is the commit message for.*:" "$FILE" | grep -oE '[a-f0-9]{7,}' | head -1)

if [ -z "$COMMIT_HASH" ]; then
    # Alternative method to get hash
    COMMIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "")
fi

CURRENT_MSG=$(head -n1 "$FILE" 2>/dev/null)

# Only replace if message contains "latest"
if echo "$CURRENT_MSG" | grep -qi "latest"; then
    case "$COMMIT_HASH" in
        213fb85) echo "feat(ui): enable integrations button and add layout classes" ;;
        c396c6b) echo "feat(db): add database deduplication and fix integrations UI" ;;
        b8547a7) echo "feat: initialize generaldb.json and add integrations color fetching" ;;
        daf6f17) echo "chore: clean up .history files and add integrations styling" ;;
        ba608e9) echo "build: rebuild project and update Main.hs" ;;
        2a6e992) echo "refactor: update handler signatures to use stateRef" ;;
        462c040) echo "build: initialize build cache configuration" ;;
        4d6e80e) echo "feat: add minecontrol-hs submodule and update build config" ;;
        fe3c984) echo "feat(users): update user management and rebuild" ;;
        60db92e) echo "refactor: update route handlers and rebuild" ;;
        32fa066) echo "feat: update Main.hs and rebuild project" ;;
        b7a0f8d) echo "feat: initialize project with UI templates and server structure" ;;
        8135c9c) echo "build: update cabal configuration with dependencies" ;;
        578a819) echo "docs: update README and rebuild binaries" ;;
        *) 
            echo "# No mapping found for $COMMIT_HASH"
            cat "$FILE"
            exit 0
            ;;
    esac > "$FILE"
    
    # Add original message as comment
    echo "" >> "$FILE"
    echo "# Original message: $CURRENT_MSG" >> "$FILE"
fi
