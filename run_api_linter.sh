#!/bin/bash

lint_files() {
  echo "$@:"
  api-linter \
    -Iapi \
    -Ilib \
    -Ithird_party/googleapis \
    -Icomponents/automate-grpc/ \
    -Ivendor/github.com/envoyproxy/protoc-gen-validate \
    -Ivendor/github.com/grpc-ecosystem/grpc-gateway/ \
    --output-format yaml \
    "$@"
}

lint_files "$@"
