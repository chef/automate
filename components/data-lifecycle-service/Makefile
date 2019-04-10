PACKAGE_PATH = github.com/chef/automate/components/data-lifecycle-service
BINS = ${PACKAGE_PATH}/cmd/data-lifecycle-service ${PACKAGE_PATH}/dls-e2eintegration

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

${BINS}: ${SERVICE_DATA_FILE} echo-build-data
	@echo "GO $@"
	@cd bin; go build ${GO_LDFLAGS} $@

serve:
	@go run ${GO_LDFLAGS} cmd/data-lifecycle-service/data-lifecycle-service.go serve ./config.dev.toml
.PHONY: serve

unit:
	@go test --count 1 -v $(shell go list ./... | grep -v '/vendor/' | grep -v 'integration') -cover
.PHONY: unit

test-ingest-purge:
	@go test -tags=cfgmgmt -v ./test/purge_ingest_integration_test.go
.PHONY: test-cfgmgmt

clean:
	@rm -rf bin/*
.PHONY: clean
