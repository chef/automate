#!/bin/bash

basedir=$(dirname "$BASH_SOURCE")
required_golang_major_minor=$(cut -f1-2 -d. <"$basedir/../GOLANG_VERSION")
have_golang_major_minor=$(go version | grep -o -E 'go[^[:space:]]+'| grep -o -E '[[:digit:]]+\.[[:digit:]]+')
if ! [ "${required_golang_major_minor}" == "${have_golang_major_minor}" ]; then
	echo "$0:"
	echo "Unsupported version of go. You have ${have_golang_major_minor}; should have ${required_golang_major_minor}"
  exit 1
fi
