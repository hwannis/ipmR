#!/usr/bin/env bash
# Build (or update) the IPM app image from GitHub - no git, unzip or file copying needed.
# Usage, on the server:   sudo bash update.sh            # latest release
#                         sudo bash update.sh v0.4.4     # a given release (or a branch name, e.g. main)
# Then recreate the container: OMV > Services > Compose > Files > ipm > Up.
set -euo pipefail
REPO="hwannis/ipmR"
REF="${1:-}"
if [ -z "$REF" ]; then
  REF=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | grep -m1 '"tag_name"' | cut -d'"' -f4 || true)
  REF="${REF:-main}"
fi
WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
echo ">> Downloading $REPO $REF"
curl -fsSL "https://github.com/$REPO/archive/$REF.tar.gz" | tar -xz -C "$WORK" --strip-components=1
grep -m1 '^Version:' "$WORK/DESCRIPTION"
echo ">> Building image ipm-app:latest (first time 10-15 min; afterwards under a minute)"
docker build -f "$WORK/deploy/docker/Dockerfile" -t ipm-app:latest -t "ipm-app:$REF" "$WORK"
echo ">> Done: ipm-app:latest = $REF. Now press Up on the 'ipm' compose file in OMV to restart the app with it."
