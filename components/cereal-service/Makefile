PACKAGE_PATH = github.com/chef/automate/components/cereal-service
BINS = ${PACKAGE_PATH}/cmd/cereal-service

GIT_SHA = $(shell git rev-parse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
VERSION = ${BUILD_TIME}
GO_LDFLAGS = --ldflags "-X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA} -X ${LIBRARY_PATH}/version.BuildTime=${BUILD_TIME}"
GOPATH = $(shell go env GOPATH)

build: ${BINS}

include ../../Makefile.common_go

echo-build-data:
	@echo "git_sha: ${GIT_SHA}"
	@echo "version: ${VERSION}"

unit:
	@go test --count 1 -v $(shell go list ./... | grep -v '/vendor/' | grep -v 'integration') -cover
.PHONY: unit

PG_URL ?= "postgresql://postgres@127.0.0.1:5432/cereal_test?sslmode=disable&timezone=UTC"
PG_ADMIN_URL ?= "postgresql://postgres@127.0.0.1:5432/template1?sslmode=disable&timezone=UTC"

integration:
	@PG_URL=$(PG_URL) PG_ADMIN_URL=$(PG_ADMIN_URL) go test -tags=integration -v ./integration/...
.PHONY: integration

clean:
	@rm -rf bin/*
.PHONY: clean
