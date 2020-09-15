// +build vendor

package main

// This file exists to trick "go mod" into including packages we need but aren't
// "imported" directly. It has no other purpose and is not inteded to be run.

import (
	_ "github.com/envoyproxy/protoc-gen-validate"
	_ "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway"
)

func main() {}
