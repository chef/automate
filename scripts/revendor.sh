#!/bin/bash
#
# Creates the vendor/ directory in this repository. Should be run from
# the root of the repository.
#
grpcGatewayVendorPath=./vendor/github.com/grpc-ecosystem/grpc-gateway/
googleAPIsVendorPath=./vendor/github.com/googleapis/googleapis/google/api
googleAPIsSubsetToKeep=google/api

echo "Vendoring dependencies in vendor/"
go mod tidy
go mod vendor
go mod verify

# Some of our tests and proto generators require files that go doesn't
# believe in vendoring so we copy them from the module cache to the
# vendor directory ourselves when we revendor.
#
# See https://github.com/golang/go/issues/26366
grpcGatewayModPath=$(GOFLAGS="" go list -f "{{.Dir}}" -m "github.com/grpc-ecosystem/grpc-gateway")

# have to `go get` it first for some reason
go get github.com/googleapis/googleapis
googleAPIsModPath=$(GOFLAGS="" go list -f "{{.Dir}}" -m "github.com/googleapis/googleapis")

# Add files that go mod won't vendor that we need
mkdir -p $grpcGatewayVendorPath
mkdir -p "$googleAPIsVendorPath"

if [[ -n "$grpcGatewayModPath" ]]; then
    cp -rf --no-preserve=mode "$grpcGatewayModPath/"* $grpcGatewayVendorPath
else
    echo "Could not find github.com/grpc-ecosystem/grpc-gateway module path"
    exit 1
fi

if [[ -n "$googleAPIsVendorPath" ]]; then
    cp -rf --no-preserve=mode "$googleAPIsModPath/$googleAPIsSubsetToKeep/"* $googleAPIsVendorPath
else
    echo "Could not find github.com/googleapis/googleapis module path"
    exit 1
fi

echo "Cleaning up unnecessary files from vendor/"
# Clean up files that go mod will vendor that we don't need
find ./vendor -type f \( -name .gitignore -o -name .travis.yml -o -name package.json -o -name Makefile -o -name Dockerfile -o -name MAINTAINERS -o -name \*.md -o -name \*.vim -o -name \*.yml \) -delete

# Explicitly clean out grpc-gateway example folder
rm -rf ./vendor/github.com/grpc-ecosystem/grpc-gateway/examples/

# Update .bldr with new dep information
echo "Regenerating .bldr configuration file"
go run tools/bldr-config-gen/main.go
