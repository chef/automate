#!/bin/bash -e

set -x
for i in $(find components/compliance-service -name '*.proto') ; do
  protoc -I. \
    -I$PWD/components/compliance-service/api \
    -I$GOPATH/src \
    -I$PWD/vendor \
    -I$PWD/vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc:$GOPATH/src \
    --grpc-gateway_out=request_context=true,logtostderr=true:"$GOPATH/src" \
    --a2-config_out=$GOPATH/src \
    $i
done
