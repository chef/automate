#!/bin/bash
set -e

type -P "gocovmerge" || go get -u github.com/wadey/gocovmerge

for pkg in $(go list ./cmd/... ./pkg/...); do
    pkg_name=$(basename $pkg)
    go test -coverprofile=cover/${pkg_name}.coverage.out $pkg
done

gocovmerge cover/*.coverage.out > cover/cover.out
echo "Coverage report written to cover/cover.out"
echo "To view:"
echo ""
echo "    go tool cover -html=cover/cover.out"
