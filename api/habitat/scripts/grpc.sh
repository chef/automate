#!/bin/bash

echo "Generate Habitat message protos for Go"

IMPORTS=(-I.)

protos=(`find "api/habitat" -maxdepth 1 -name "*.proto"`)
if [ ${#protos[@]} -gt 0 ]; then
  list=(api/habitat/*.proto)
  printf 'GEN: %s\n' "${list[@]}"

  protoc ${IMPORTS[@]} \
    --go_out=. \
    ${list[@]} || exit 1

fi
