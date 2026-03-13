#!/bin/sh
set -eu

# Configure this repository to use the versioned hooks in .githooks/
# Usage: ./setup-githooks.sh

REPO_ROOT=$(cd "$(dirname "$0")" && pwd)
cd "$REPO_ROOT"

if ! command -v git >/dev/null 2>&1; then
  echo "git not found in PATH" >&2
  exit 1
fi

# Ensure we're in a git worktree
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || {
  echo "Not inside a git repository: $REPO_ROOT" >&2
  exit 1
}

# Make hook scripts executable (important on unixy systems)
chmod +x .githooks/pre-commit .githooks/post-merge .githooks/post-checkout 2>/dev/null || true

git config core.hooksPath .githooks

echo "Configured core.hooksPath=.githooks for $(git rev-parse --show-toplevel)"
