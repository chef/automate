#!/bin/bash

set -e

###
# ==BOOTSTRAP PROTOC==
###
# This script compiles enough protos for protoc-gen-policy and
# protoc-gen-a2-configto be built, which enables us to subsequently compile all
# the rest of the protocol buffers. This script needs to be able to run in a
# completely clean checkout of the repo with all the compiled protos removed
# (i.e., all files with '*pb*go' extensions deleted).


printf 'GEN: %s\n' components/automate-grpc/protoc-gen-policy/api/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-policy/api/*.proto

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-policy/iam/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-policy/iam/*.proto

