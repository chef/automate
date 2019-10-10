#!/bin/bash

set -e

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-policy/api/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-policy/api/*.proto

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-policy/iam/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-policy/iam/*.proto

printf 'GEN: %s\n' lib/grpc/debug/debug_api/*.proto
protoc -I /src --go_out=plugins=grpc,paths=source_relative:/src \
  --policy_out=logtostderr=true,paths=source_relative:/src \
  lib/grpc/debug/debug_api/*.proto

printf 'GEN: %s\n' components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
protoc -I /src --go_out=logtostderr=true,paths=source_relative:/src \
  components/automate-grpc/protoc-gen-a2-config/api/a2conf/*.proto
