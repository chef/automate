#!/bin/bash

set -e

pushd "${BASH_SOURCE%/*}/.."

make proto

popd

