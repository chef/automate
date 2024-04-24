#!/bin/bash
#
# This script wraps the golang and protobuf dependency maintenance/cleanup
# tasks for the entire Automate repo.
#
# For the main automate module, it should suffice to use standard go
# module-based dependency management commands. For the protovendor tree/module,
# we use the go tooling to fetch protobuf repos that we have vendored. This use
# case isn't currently supported directly by the go module system which we work
# around with the code here.
#
# Originally Automate was one big go module and go dependencies were vendored
# along with the protobufs. Any weirdness in this script might be a result of
# the large changes in dependency management strategy since then.
#
grpcGatewayVendorPath=./github.com/grpc-ecosystem/grpc-gateway/
googleAPIsVendorPath=./github.com/googleapis/googleapis/google/api
googleAPIsSubsetToKeep=google/api
envoyproxyVendorPath=./github.com/envoyproxy/protoc-gen-validate

# This has the side effect of downloading all the modules, which needs to
# happen before we can copy the protos from those modules to protovendor/
echo "Tidying and Verifying Go Modules in Automate Main Module"
go mod tidy
go mod verify

pushd protovendor > /dev/null || (echo "couldn't enter the protovendor dir from $(pwd)" && exit 1)

echo "Tidying and Verifying Go Modules in Automate Protovendor Module"
go mod tidy
go mod verify

echo "Vendoring protos in protovendor/ ..."

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

echo "Cleaning up unnecessary files from protovendor/github.com/"
find github.com -not -name "*proto" -type f -exec rm '{}' \;

# Explicitly clean out grpc-gateway example folder
rm -rf ./github.com/grpc-ecosystem/grpc-gateway/examples/

popd > /dev/null || exit 1

# Update .bldr with new dep information
echo "Regenerating .bldr configuration file"
go run tools/bldr-config-gen/main.go
