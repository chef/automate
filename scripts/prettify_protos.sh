#!/bin/bash

find api components -name '*.proto' \
     -not -path '*components/notifications-service/server/_build*' \
     -not -path '*components/automate-grpc/protoc-gen-policy/testdata/*' \
     -exec go run tools/pretty-proto/main.go {} +;
