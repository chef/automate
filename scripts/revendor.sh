#!/bin/bash
#
# Creates the protovendor/ directory in this repository. Should be run from
# the root of the repository.
#
grpcGatewayVendorPath=./protovendor/github.com/grpc-ecosystem/grpc-gateway/
googleAPIsVendorPath=./protovendor/github.com/googleapis/googleapis/google/api
googleAPIsSubsetToKeep=google/api
envoyproxyVendorPath=./protovendor/github.com/envoyproxy/protoc-gen-validate

echo "Vendoring protos in protovendor/ ..."
go mod tidy
go mod verify

# Originally this script existed to copy .proto files from the go module cache
# because go mod vendor doesn't keep .proto files. Now that we are not using
# vendoring for go modules, we still use this mechanism to vendor the protos
#
# See https://github.com/golang/go/issues/26366
grpcGatewayModPath=$(GOFLAGS="" go list -f "{{.Dir}}" -m "github.com/grpc-ecosystem/grpc-gateway")
envoyproxyModPath=$(GOFLAGS="" go list -f "{{.Dir}}" -m "github.com/envoyproxy/protoc-gen-validate")

# have to `go get` it first for some reason.
# NOTE: the version here isn't particularly special, it's just the version we
# happened to get on a particular day. We have to lock to *a* version, because
# the version ends up in the go.mod file and we check whether that file is
# properly up-to-date in Ci.
go get github.com/googleapis/googleapis@a94df49e8f20
googleAPIsModPath=$(GOFLAGS="" go list -f "{{.Dir}}" -m "github.com/googleapis/googleapis")

mkdir -p $grpcGatewayVendorPath
mkdir -p "$googleAPIsVendorPath"
mkdir -p "$envoyproxyVendorPath"

if [[ -n "$grpcGatewayModPath" ]]; then
    cp -rf --no-preserve=mode "$grpcGatewayModPath/"* $grpcGatewayVendorPath
else
    echo "Could not find github.com/grpc-ecosystem/grpc-gateway module path"
    exit 1
fi

if [[ -n "$envoyproxyModPath" ]]; then
    cp -rf --no-preserve=mode "$envoyproxyModPath/"* $envoyproxyVendorPath
else
    echo "Could not find github.com/envoyproxy/protoc-gen-validate module path"
    exit 1
fi


if [[ -n "$googleAPIsVendorPath" ]]; then
    cp -rf --no-preserve=mode "$googleAPIsModPath/$googleAPIsSubsetToKeep/"* $googleAPIsVendorPath
else
    echo "Could not find github.com/googleapis/googleapis module path"
    exit 1
fi

echo "Cleaning up unnecessary files from protovendor/"
find ./protovendor -type f \( -name .gitignore -o -name .travis.yml -o -name package.json -o -name Makefile -o -name Dockerfile -o -name MAINTAINERS -o -name \*.md -o -name \*.vim -o -name \*.yml \) -delete

# Explicitly clean out grpc-gateway example folder
rm -rf ./protovendor/github.com/grpc-ecosystem/grpc-gateway/examples/

# Update .bldr with new dep information
echo "Regenerating .bldr configuration file"
go run tools/bldr-config-gen/main.go
