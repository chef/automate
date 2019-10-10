#!/bin/bash
set -e

protoc_repo=github.com/golang/protobuf/protoc-gen-go
protoc_version="3.9.2"

[[ $(type -P "protoc") ]] || (echo "unable to proceed. protobuf is not installed" && exit 1)
protoc --version | grep -q $protoc_version  || (echo "unable to proceed. protobuf must be version $protoc_version" && exit 1)
[[ $(type -P "protoc-gen-go") ]] || go get -u ${protoc_repo}

# leave the loop in so it will be easier if we need new stuff in the future
# shellcheck disable=SC2043
shopt -s globstar
for pkg in api/config/**/; do
  proto_files=($(find "$pkg" -maxdepth 1 -name "*.proto"))

  if [ ${#proto_files[@]} -gt 0 ]; then
      printf 'GEN: %s\n' "${proto_files[*]}"

      protoc -I /src \
          --go_out=plugins=grpc,paths=source_relative:/src \
          --a2-config_out=paths=source_relative:/src \
          ${proto_files[*]} # don't quote so we word split

      pb_files=$(ls "$pkg"/*.pb.go)

      # extract the first field from the json tag (the name) and
      # apply it as the contents of the toml tag
      for i in ${pb_files}; do
          # We are assuming GNU sed. It doesn't work on OS X--use the hab studio
          sed -i 's/json:"\([^,]*\).*"/& toml:"\1,omitempty" mapstructure:"\1,omitempty"/;s/toml:"-,omitempty"/toml:"-"/' "$i"
      done
  fi
done
