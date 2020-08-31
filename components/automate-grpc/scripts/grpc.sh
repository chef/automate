#!/bin/bash

set -e

###
# NOTE: grpc.sh vs. bootstrap_protoc.sh
###
# This script compiles all the protos. It is normally what you want. This
# script is allowed to depend on tools in this repo (e.g.,
# protoc-gen-a2-config, etc.)

# shellcheck disable=SC1090
source "${BASH_SOURCE%/*}/bootstrap_protoc.sh"

pushd lib

printf 'GEN: %s\n' lib/grpc/debug/debug_api/*.proto
protoc -I /src/components -I /src/lib  -I /src/api --go_out=plugins=grpc,paths=source_relative:/src/lib \
  --policy_out=logtostderr=true,paths=source_relative:/src/lib \
  grpc/debug/debug_api/*.proto

popd

pushd components

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
protoc -I /src/components -I /src/lib  -I /src/api --go_out=paths=source_relative:/src/components \
  automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto

popd
