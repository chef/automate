#!/bin/bash

set -e

printf 'GEN: %s\n' lib/license/*.proto
protoc -I /src --go_out=paths=source_relative:/src \
  lib/license/*.proto

