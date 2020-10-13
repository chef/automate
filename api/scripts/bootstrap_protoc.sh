#!/bin/bash

pushd /src/api >> /dev/null || exit 1
printf 'GEN: %s\n' api/external/annotations/iam/*.proto
protoc -I /src/api --go_out=paths=source_relative:/src/api \
  external/annotations/iam/*.proto || exit 1
popd >> /dev/null || exit 1
