#!/bin/bash
# set -x
GOPATH=$(go env GOPATH)

echo "Generate Go, Swagger, and GRPC Gateway"

IMPORTS=(-I.
         -I"$GOPATH/src"
         -Ivendor
         -Ivendor/github.com/grpc-ecosystem/grpc-gateway
         -Ivendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis
         -Ivendor/github.com/envoyproxy/protoc-gen-validate
        )

# globstar: makes **/ resolve nested directories
# nullglob: make empty-result-glob yield empty-array ($list below)
shopt -s globstar nullglob

for i in components/automate-gateway/api/**/; do
  # check if there are proto file is in that directory
  list=("$i"*.proto)
  if [ ${#list[@]} -ne 0 ]; then
    printf 'GEN:'
    printf ' %s' "${list[@]}"
    printf '\n'
    thisfile=$(basename "${list[0]}") # only used to skip to specific files below

    # Can't yet generate mocks for a few of the protos since they
    # have some conventions the mocks can't handle yet (stream keyword for example).
    if [ "$thisfile" == "reporting.proto" ] || [ "$thisfile" == "profiles.proto" ]; then
      protoc "${IMPORTS[@]}" \
        --go_out=plugins=grpc:"$GOPATH/src" \
        --grpc-gateway_out=request_context=true,logtostderr=true:"$GOPATH/src" \
        --policy_out=logtostderr=true:"$GOPATH/src" \
        "${list[@]}" || exit 1
    else
      # service, grpc-gateway, policy mapping
      protoc "${IMPORTS[@]}" \
        --go_out=plugins=grpc:"$GOPATH/src" \
        --grpc-gateway_out=request_context=true,logtostderr=true:"$GOPATH/src" \
        --policy_out=logtostderr=true:"$GOPATH/src" \
        --grpc-mock_out="$GOPATH/src" \
        --a2-config_out=$GOPATH/src \
        "${list[@]}" || exit 1
    fi

    # generates swagger output, only generate if a gateway file was generated
    gw_files=("$i"*.gw.go)
    if [ ${#gw_files[@]} -ne 0 ]; then
        protoc "${IMPORTS[@]}" --swagger_out=logtostderr=true,fqn_for_swagger_name=true:"$PWD" "${list[@]}" || exit 1
    fi
  fi
done

echo -e "\\nLinting proto files..."
for i in components/automate-gateway/api/**/**/; do
  list=("$i"*.proto)
  if [ ${#list[@]} -ne 0 ]; then
      printf 'LINT:'
      printf ' %s' "${list[@]}"
      printf '\n'
      nolint=$(grep "^//.*nolint" ${list[@]})
      if [ $? -eq 0 ]; then
          echo ${nolint}
          protoc "${IMPORTS[@]}" --lint_out="$PWD" "${list[@]}" || echo
      else
          protoc "${IMPORTS[@]}" --lint_out="$PWD" "${list[@]}" || exit 1
      fi
  fi
done

# This script reads all swagger '.json' files in the current folder
# and encodes them as strings literals in swagger.pb.go
#
# TODO (@afiune) Should this be moved to a 'go generate' hook instead?
echo -e "\\nCreating go src for swagger"
pushd "${scaffolding_go_pkg_path:-$PWD}/components/automate-gateway/api" >/dev/null
go run scripts/swagger.go
popd >/dev/null
