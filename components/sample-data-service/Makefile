include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/sample-data-service

BINS = ${PACKAGE_PATH}/cmd/sample-data-service

packages:=${PACKAGE_PATH}/...
ifdef CI
  verbose?="-v"
endif

all: lint build test

static: lint
unit: build test

build:
	mkdir -p bin/
	go build -o bin/sample-data-service ./cmd/sample-data-service

.PHONY: all static unit build compile proto test dep-ensure have-dep lint
