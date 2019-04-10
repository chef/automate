#!/bin/bash -e

set -x
for i in $(find components/nodemanager-service -name '*.proto') ; do
  protoc -I. \
    -I$PWD/components/nodemanager-service/api \
    -I$GOPATH/src \
    -I$PWD/vendor \
    -I$PWD/vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc:$GOPATH/src \
    --a2-config_out=$GOPATH/src \
    $i
done
