#!/bin/bash
# set -x
GOPATH=$(go env GOPATH)

echo "Generate Go, Swagger, and GRPC Gateway"

IMPORTS=(-I.
         -I$GOPATH/src
         -Ivendor
         -Ivendor/github.com/grpc-ecosystem/grpc-gateway
         -Ivendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis
         -Ivendor/github.com/envoyproxy/protoc-gen-validate
         -Ilib/license
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
      --go_out=plugins=grpc:$GOPATH/src \
      --grpc-gateway_out=request_context=true,logtostderr=true:$GOPATH/src  \
      --policy_out=logtostderr=true:$GOPATH/src  \
      ${list[@]} || exit 1

    # generates swagger output, only generate if a gateway file was generated
    gogw=(`find $i -maxdepth 1 -name "*.gw.go"`)
    if [ ${#gogw[@]} -gt 0 ]; then
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
      --go_out=plugins=grpc:$GOPATH/src \
      --grpc-gateway_out=request_context=true,logtostderr=true:$GOPATH/src  \
      ${list[@]} || exit 1

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
                   --grpc-mock_out=$GOPATH/src \
                   --validate_out=lang=go:$GOPATH/src \
                   ${list[@]} || exit 1
            ;;
        *)
            # no validation
            ;;
    esac
    # generates swagger output, only generate if a gateway file was generated
    gogw=(`find $i -maxdepth 1 -name "*.gw.go"`)
    if [ ${#gogw[@]} -gt 0 ]; then
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
pushd ${scaffolding_go_pkg_path:-$PWD}/api > /dev/null
go run scripts/swagger.go
popd > /dev/null
