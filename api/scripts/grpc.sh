#!/bin/bash
# set -x

echo "Annotate authorization actions"

doc_action() { awk -f ./scripts/add_action_doc_to_proto.awk $1 > data.tmp && mv data.tmp $1 ; }
export -f doc_action
grep --include=\*.proto -rwl rpc api | xargs grep -l '\*\/' | xargs -t -n1 -P1 bash -c 'doc_action "$@"' _

echo "Generate Go, Swagger, Validation, and GRPC Gateway"

# Unlike gen-go, the gateway, swagger, and validation protoc extension don't
# support the source_relative path option, which is crucial when generating
# go code in go module mode without a full GOPATH. Therefore, we have to use
# the default import path option. Since we're using modules and don't have
# a GOPATH we'll create a faux GOPATH and copy the generated files into the
# the proper source when the source_relative path option is not available.

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

IMPORTS=(-I /src
         -I /src/api
         -I vendor
         -I vendor/github.com/grpc-ecosystem/grpc-gateway
         -I vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis
         -I vendor/github.com/envoyproxy/protoc-gen-validate
         -I lib/license
        )

shopt -s globstar
for i in api/external/**/; do
  # check if there are proto files in that directory
  protos=(`find "$i" -maxdepth 1 -name "*.proto"`)
  if [ ${#protos[@]} -gt 0 ]; then
    list=($i*.proto)
    printf 'GEN: %s\n' "${list[@]}"

    # service, grpc-gateway, policy mapping
    protoc ${IMPORTS[@]} \
      --go_out=plugins=grpc,paths=source_relative:/src \
      --grpc-gateway_out=request_context=true,logtostderr=true:$fauxpath  \
      --policy_out=logtostderr=true:$fauxpath  \
      ${list[@]} || exit 1

    sync_from_fauxpath "${list[0]}"

    # generates swagger output, only generate if a gateway file was generated
    gogw=(`find $i -maxdepth 1 -name "*.gw.go"`)
    if [ ${#gogw[@]} -gt 0 ]; then
      printf 'SWG: %s\n' "${list[@]}"
      protoc  ${IMPORTS[@]} --swagger_out=logtostderr=true,fqn_for_swagger_name=true:$PWD ${list[@]} || exit 1
    fi
  fi
done

# This is a separate loop because we do not want to do policy generation
for i in api/interservice/**/; do
  # check if there are proto files in that directory
  protos=(`find "$i" -maxdepth 1 -name "*.proto"`)
  if [ ${#protos[@]} -gt 0 ]; then
    list=($i*.proto)
    printf 'GEN: %s\n' "${list[@]}"

    # service, grpc-gateway, policy mapping
    protoc ${IMPORTS[@]} \
      --go_out=plugins=grpc,paths=source_relative:/src \
      --grpc-gateway_out=request_context=true,logtostderr=true:$fauxpath  \
      ${list[@]} || exit 1

    sync_from_fauxpath "${list[0]}"

    # Validation & Mocks
    #
    # TODO(ssd) 2019-02-28: Not all protos were listed with the
    # validation and mock plugin in mind. Including validations for
    # every plugin would be problematic as the Validate() functions
    # can conflict. For now we whitelist a set of them:
    case $i in
        "api/interservice/local_user/"*|\
        "api/interservice/teams/"*|\
        "api/interservice/authn/"*|\
        "api/interservice/authz/"*)
            printf 'VAL: %s\n' "${list[@]}"
            protoc ${IMPORTS[@]} \
                   --grpc-mock_out="${fauxpath}" \
                   --validate_out=lang=go:"${fauxpath}" \
                   ${list[@]} || exit 1

            sync_from_fauxpath "${list[0]}"
            ;;
        *)
            # no validation
            ;;
    esac
    # generates swagger output, only generate if a gateway file was generated
    gogw=(`find $i -maxdepth 1 -name "*.gw.go"`)
    if [ ${#gogw[@]} -gt 0 ]; then
      printf 'SWG: %s\n' "${list[@]}"
      protoc  ${IMPORTS[@]} --swagger_out=logtostderr=true,fqn_for_swagger_name=true:$PWD ${list[@]} || exit 1
    fi

    pb_files=$(ls "$i"/*.pb.go)
    # extract the first field from the json tag (the name) and apply it as the
    # contents of the toml tag
    for i in ${pb_files}; do
        sed -i 's/json:"\([^,]*\).*"/& toml:"\1,omitempty" mapstructure:"\1,omitempty"/;s/toml:"-,omitempty"/toml:"-"/' "$i"
    done
  fi
done


# This script reads all swagger '.json' files in the current folder
# and encodes them as strings literals in swagger.pb.go
#
# TODO (@afiune) Should this be moved to a 'go generate' hook instead?
echo " "
echo "Creating go src for swagger"
pushd "/src/api" || exit 1
go run scripts/swagger.go
popd || exit 1
