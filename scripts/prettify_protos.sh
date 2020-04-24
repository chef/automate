#!/bin/bash

find api components -name '*.proto' \
     -not -path '*components/notifications-service/server/priv/notifications.proto' \
     -not -path '*components/notifications-service/server/priv/rules.proto' \
     -not -path '*components/notifications-service/server/priv/health.proto' \
     -not -path '*components/notifications-service/server/_build*' \
     -not -path '*components/automate-grpc/protoc-gen-policy/testdata/*' \
     -exec go run tools/pretty-proto/main.go {} +;

go run tools/pretty-proto/main.go \
   components/notifications-service/server/priv/notifications.proto \
   components/notifications-service/server/priv/rules.proto \
   components/notifications-service/server/priv/health.proto
