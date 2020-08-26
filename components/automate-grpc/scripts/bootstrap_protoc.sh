#!/bin/bash

set -e

###
# ==BOOTSTRAP PROTOC==
###
# This script compiles enough protos for protoc-gen-policy and
# protoc-gen-a2-config to be built, which enables us to subsequently compile all
# the rest of the protocol buffers. This script needs to be able to run in a
# completely clean checkout of the repo with all the compiled protos removed
# (i.e., all files with '*pb*go' extensions deleted).

pushd components

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
protoc -I /src/components --go_out=paths=source_relative:/src/components \
  automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto

popd
