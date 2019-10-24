#!/bin/bash

# Unlike gen-go, the gateway protoc extension doesn't support the source_relative
# path option, which is crucial when generating go code in go module mode without
# a full GOPATH. Therefore, we have to use the default import path option. Since
# we're using modules and don't have a GOPATH we'll create a faux GOPATH and copy
# the generated files into the the proper source when the source_relative path
# option is not available.

fauxpath=$(mktemp -d)
[[ ! "$fauxpath" || ! -d "$fauxpath" ]] && echo "Unable to create temp directory" && exit 1

# Given a proto file location, copy the generated output from the faux GOPATH to
# the src directory.
function sync_from_fauxpath() {
    base_dir=$(dirname "${1}")
    gen_dir="${fauxpath}/github.com/chef/automate/${base_dir}/"
    [[ -d "${gen_dir}" ]] && rsync -r "${gen_dir}" "/src/${base_dir}/"
}

function cleanup() {
  [[ -d "${fauxpath}" ]] && rm -rf "${fauxpath}"
}
trap cleanup EXIT

for i in $(find components/compliance-service -name '*.proto') ; do
  protoc -I /src \
    -I src/components/compliance-service/api \
    -I vendor \
    -I vendor/github.com/grpc-ecosystem/grpc-gateway \
    -I vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
    --go_out=plugins=grpc,paths=source_relative:/src \
    --grpc-gateway_out=request_context=true,logtostderr=true:"${fauxpath}" \
    --a2-config_out=paths=source_relative:/src \
    "${i}"
  printf 'GEN: %s\n' "${i}"

  sync_from_fauxpath "${i}"
done
