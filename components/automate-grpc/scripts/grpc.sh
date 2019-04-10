#!/bin/bash
set -x
GOPATH=$(go env GOPATH)

protoc --go_out=logtostderr=true:$GOPATH/src \
  components/automate-grpc/protoc-gen-policy/api/*.proto
protoc --go_out=logtostderr=true:$GOPATH/src \
  components/automate-grpc/protoc-gen-policy/iam/*.proto
protoc --go_out=plugins=grpc:$GOPATH/src \
  --policy_out=logtostderr=true:$GOPATH/src \
  lib/grpc/debug/debug_api/*.proto

protoc --go_out=logtostderr=true:$GOPATH/src \
  components/automate-grpc/protoc-gen-a2-config/api/**/*.proto
