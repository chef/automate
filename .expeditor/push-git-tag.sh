#!/bin/bash

set -euo pipefail

manifest_url="https://packages.chef.io/manifests/${EXPEDITOR_TARGET_CHANNEL}/automate/latest.json"
echo "Downloading latest manifest from $manifest_url"
curl -o manifest.json "$manifest_url"

echo "Parsing manifest for git SHA and build"
git_sha="$(jq -r -e ".git_sha" manifest.json)"
build_version="$(jq -r -e ".build" manifest.json)"

if [[ -z "$git_sha" ]]; then
    echo "No git sha in latest manifest:"
    cat manifest.json
    exit 1
fi

if [[ -z "$build_version" ]]; then
    echo "No build version in latest manifest:"
    cat manifest.json
    exit 1
fi

git tag "$build_version" "$git_sha"
git push origin "$build_version"
