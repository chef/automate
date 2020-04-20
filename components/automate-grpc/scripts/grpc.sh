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

printf 'GEN: %s\n' lib/grpc/debug/debug_api/*.proto
protoc -I /src --go_out=plugins=grpc,paths=source_relative:/src \
  --policy_out=logtostderr=true,paths=source_relative:/src \
  lib/grpc/debug/debug_api/*.proto

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
