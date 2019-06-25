include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/local-user-service

BINS = ${PACKAGE_PATH}/cmd/users ${PACKAGE_PATH}/cmd/users-health

GIT_SHA = $(shell git rev-parse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
GO_LDFLAGS = --ldflags "-X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA} -X ${LIBRARY_PATH}/version.BuildTime=${BUILD_TIME}"

packages:=${PACKAGE_PATH}/...

all: lint build test

static: lint
unit: build test

${BINS}: bin
	@echo "GO $@"
	@cd bin; go build ${GO_LDFLAGS} $@

bin:
	mkdir -p bin

build: ${BINS}

test:
	@go test -i $(packages)
	@go test $(packages) -cover

# Regenerate all *.pb.go files
proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component api'

.PHONY: all fmt test vet static unit
