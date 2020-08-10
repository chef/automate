// +build vendor

package main

// This file exists to trick "go mod" into including packages we need but aren't
// "imported" directly. It has no other purpose and is not inteded to be run.

import (
	_ "github.com/ckaznocha/protoc-gen-lint"
	_ "github.com/envoyproxy/protoc-gen-validate"
	_ "github.com/go-delve/delve/cmd/dlv"
	_ "github.com/golang/mock/gomock"
	_ "github.com/golang/mock/mockgen"
	_ "github.com/golang/protobuf/protoc-gen-go"
	_ "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway"
	_ "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger"
	_ "github.com/kevinburke/go-bindata/go-bindata"
	_ "golang.org/x/perf/cmd/benchstat"
	_ "github.com/bufbuild/buf/cmd/buf"
	_ "github.com/bufbuild/buf/cmd/protoc-gen-buf-check-breaking"
	_ "github.com/bufbuild/buf/cmd/protoc-gen-buf-check-lint"
)

func main() {}
