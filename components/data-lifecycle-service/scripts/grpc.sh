#!/bin/bash

protoc -I. \
  -I $GOPATH/src \
  -I vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
  --go_out=plugins=grpc:$GOPATH/src \
  --a2-config_out=$GOPATH/src \
  components/data-lifecycle-service/api/*.proto

for i in $(ls components/data-lifecycle-service/api/*.pb.go); do
  sed -i 's/json:"\([^,]*\).*"/& toml:"\1,omitempty" mapstructure:"\1"/;s/toml:"-,omitempty"/toml:"-"/' "$i"
done
