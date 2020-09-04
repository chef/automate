#!/bin/bash
# set -x

echo "Annotate authorization actions"

doc_action() { awk -f ./scripts/add_action_doc_to_proto.awk $1 > data.tmp && mv data.tmp $1 ; }
export -f doc_action
grep --include=\*.proto -rwl rpc components/automate-gateway | xargs grep -l '\*\/' | xargs -t -n1 -P1 bash -c 'doc_action "$@"' _

echo "Generate Go, Swagger, and GRPC Gateway"

# Unlike gen-go, the gateway and swagger protoc extensions don't support the
# source_relative path option, which is crucial when generating go code in go
# module mode without a full GOPATH. Therefore, we have to use the default
# import path option. Since we're using modules and don't have a GOPATH we'll
# create a faux GOPATH and copy the generated files into the the proper source
# when the source_relative path option is not available.

fauxpath=$(mktemp -d)
[[ ! "$fauxpath" || ! -d "$fauxpath" ]] && echo "Unable to create temp directory" && exit 1

# Given a proto file location, copy the generated output from the faux GOPATH to
# the src directory.
function sync_from_fauxpath() {
    base_dir=$(dirname "${1}")
    gen_dir="${fauxpath}/github.com/chef/automate/components/${base_dir}/"
    dest_dir="/src/components/${base_dir}/"
    [[ -d "${gen_dir}" ]] && rsync -r "${gen_dir}" "${dest_dir}"
}

function cleanup() {
  [[ -d "${fauxpath}" ]] && rm -rf "${fauxpath}"
}
trap cleanup EXIT

IMPORTS=(
 -I /src/api
 -I /src/components
 -I /src/protovendor
 -I /src/protovendor/github.com/grpc-ecosystem/grpc-gateway
 -I /src/protovendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis
 -I /src/protovendor/github.com/ckaznocha/protoc-gen-lint
)

# globstar: makes **/ resolve nested directories
# nullglob: make empty-result-glob yield empty-array ($list below)
shopt -s globstar nullglob

pushd components
for i in automate-gateway/api/**/; do
  # check if there are proto file is in that directory
  list=("$i"*.proto)
  if [ ${#list[@]} -ne 0 ]; then
    printf 'GEN: %s\n' "${list[@]}"
    thisfile=$(basename "${list[0]}") # only used to skip to specific files below

    # Can't yet generate mocks for a few of the protos since they
    # have some conventions the mocks can't handle yet (stream keyword for example).
    if [ "$thisfile" == "reporting.proto" ] || [ "$thisfile" == "profiles.proto" ]; then
      protoc "${IMPORTS[@]}" \
        --go_out=plugins=grpc,paths=source_relative:/src/components \
        --grpc-gateway_out=request_context=true,logtostderr=true:"${fauxpath}" \
        --policy_out=logtostderr=true:"${fauxpath}" \
        "${list[@]}" || exit 1
    else
      # service, grpc-gateway, policy mapping
      protoc "${IMPORTS[@]}" \
        --go_out=plugins=grpc,paths=source_relative:/src/components \
        --grpc-gateway_out=request_context=true,logtostderr=true:"${fauxpath}" \
        --policy_out=logtostderr=true:"${fauxpath}" \
        --grpc-mock_out="${fauxpath}" \
        --a2-config_out=paths=source_relative:/src/components \
        "${list[@]}" || exit 1
    fi

    sync_from_fauxpath "${list[0]}"

    # generates swagger output, only generate if a gateway file was generated
    gw_files=$(find $i -maxdepth 1 -name "*.gw.go")
    if [ ${#gw_files[@]} -gt 0 ]; then
      protoc "${IMPORTS[@]}" --swagger_out=logtostderr=true,fqn_for_swagger_name=true:"$PWD" "${list[@]}" || exit 1
    fi
  fi
done

echo -e "\\nLinting proto files..."
for i in automate-gateway/api/**/**/; do
  list=("$i"*.proto)
  if [ ${#list[@]} -ne 0 ]; then
      printf 'LINT: %s\n' "${list[@]}"
      nolint=$(grep "^//.*nolint" ${list[@]})
      if [ $? -eq 0 ]; then
          echo ${nolint}
          protoc "${IMPORTS[@]}" --lint_out="$PWD" "${list[@]}" || echo
      else
          protoc "${IMPORTS[@]}" --lint_out="$PWD" "${list[@]}" || exit 1
      fi
  fi
done
popd

# This script reads all swagger '.json' files in the current folder
# and encodes them as strings literals in swagger.pb.go
#
# TODO (@afiune) Should this be moved to a 'go generate' hook instead?
echo -e "\\nCreating go src for swagger"
pushd /src/components/automate-gateway/api >/dev/null || exit 1
go run scripts/swagger.go
popd >/dev/null || exit 1
