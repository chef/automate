#!/bin/bash

set -e

hab pkg install -b core/go core/git core/ruby core/jq-static core/shellcheck

echo "Checking Go Dependencies"
go mod verify
git diff --exit-code --ignore-submodules=all # fail if anything's been changed

echo "Checking automate-deployment binds.txt"
(
  cd ./components/automate-deployment
  make check-bindings
)

yml2json() {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load(ARGF))' "$1"
}

echo "Checking if Golang license fallbacks/exceptions are needed"
license_scout=$(yml2json .license_scout.yml)
for d in $(jq -ner --argjson data "$license_scout" '$data | (.fallbacks, .exceptions) | (.golang // [])[].name'); do
    if [[ ! -d "vendor/$d" ]]; then
        echo "dependency \"$d\" not required anymore"
        exit 1
    fi
done

echo "Checking for up-to-date bldr config"
go run ./tools/bldr-config-gen
if ! git diff --exit-code --ignore-submodules=all; then
    echo "The bldr config appears to be out of date!"
    echo "To fix this, run:"
    echo ""
    echo "   hab studio run \"source .studiorc && generate_bldr_config\""
    echo ""
    echo "Inspect and commit the resulting changes if they look reasonable"
    exit 1
fi

echo "Shellchecking!"
shellcheck -s bash -ax \
  .expeditor/*.sh \
  .expeditor/**/*.sh \
  .buildkite/hooks/* \
  scripts/*.sh \
  integration/**/*.sh

# Applying shellcheck to the studio scripts is still in-progress.  To
# help, choose one of the violations excluded here and fix all
# instances of it.
shellcheck -s bash -ax \
  -e SC2012,SC2034,SC2046,SC2086,SC2119,SC2120,SC2124,SC2128,SC2154,SC2164,SC2181,SC2207 \
  .studiorc .studio/*

echo "Checking for possible credentials in the source code"
go run ./tools/credscan
