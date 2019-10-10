#!/bin/bash
set -e

# shellcheck disable=SC2043

protoc_repo=github.com/golang/protobuf/protoc-gen-go
protoc_version="3.9.2"

[[ $(type -P "protoc") ]] || (echo "unable to proceed. protobuf is not installed" && exit 1)
[[ $(protoc --version | grep $protoc_version) ]] || (echo "unable to proceed. protobuf must be version $protoc_version" && exit 1)
[[ $(type -P "protoc-gen-go") ]] || go get -u ${protoc_repo}

# We cannot use protoc-gen-star with the persistence package because it is not annotated with
# a package.
# TODO(jaym): add package to persistence

for i in components/automate-deployment/pkg/persistence/boltdb/internal/v*/schema; do
  proto_files=$(ls $i/*.proto)

  printf 'GEN: %s\n' "${proto_files[@]}"

  protoc -I /src --go_out=plugins=grpc,paths=source_relative:/src ${proto_files}

  pb_files=$(ls $i/*.pb.go)

  # extract the first field from the json tag (the name) and
  # apply it as the contents of the toml tag
  for f in ${pb_files}; do
    # We are assuming GNU sed. It doesn't work on OS X--use the hab studio
    sed -i 's/json:"\([^,]*\).*"/& toml:"\1,omitempty" mapstructure:"\1,omitempty"/;s/toml:"-,omitempty"/toml:"-"/' "$f"
  done
done
