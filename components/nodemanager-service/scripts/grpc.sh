#!/bin/bash -e

for i in $(find components/nodemanager-service -name '*.proto') ; do
  printf 'GEN: %s\n' "${i}"
  protoc -I /src \
    -I /src/components/nodemanager-service/api \
    -I vendor \
    -I vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc,paths=source_relative:/src \
    --a2-config_out=paths=source_relative:/src \
    --a2-config_out=paths=source_relative:/src \
    "${i}"
done
