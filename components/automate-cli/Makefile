PACKAGE_PATH ?= github.com/chef/automate/components/automate-cli
CHEF_AUTOMATE_PKG = ${PACKAGE_PATH}/cmd/chef-automate
BINS = ${CHEF_AUTOMATE_PKG}
DOCS_PATH = ../docs-chef-io/data/automate/cli_chef_automate

package: linux

LINTERARGS:=--exclude-use-default -D lll ./...
ifneq ("$(wildcard ../../Makefile.common_go)", "")
include ../../Makefile.common_go
endif

GOOS = $(shell go env GOOS)
GIT_SHA = $(shell git rev-parse HEAD)
VERSION ?= 0.1.0
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
LINKER_FLAGS = -X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA} -X ${LIBRARY_PATH}/version.BuildTime=${BUILD_TIME}
GO_LDFLAGS=--ldflags '-extldflags "-static" ${LINKER_FLAGS}'
BIN_DIR ?= bin
CROSS_TARGET_DIR ?= bin

# cross compilation targets
cross: linux darwin windows

build: ${BINS}

${BINS}: ${BIN_DIR} echo-build-data
	@echo "GO $@"
	@cd ${BIN_DIR}; go build ${GO_LDFLAGS} $@

echo-build-data:
	@echo "build_time: ${BUILD_TIME}"
	@echo "   git_sha: ${GIT_SHA}"
	@echo "   version: ${VERSION}"

${BIN_DIR}:
	mkdir -p ${BIN_DIR}

docs: clean ${BINS} ${GOOS}
	mkdir -p ${DOCS_PATH}
	# In the dev studio, CHEF_DEV_ENVIRONMENT is set to true. This has
	# the effect that further dev-only commands appear in the output of
	# chef-automate --help etc. Since it unhides commands, that'll also
	# have an effect on the documentation data generated.
	CHEF_DEV_ENVIRONMENT=false bin/${GOOS}/chef-automate dev generate-docs --docs-dir ${DOCS_PATH} --format yaml

clean:
	rm -rf bin/* "${DOCS_PATH}"

EXE = chef-automate
linux: EXTRA_OPTS = CGO_ENABLED=0
darwin: EXTRA_OPTS = CGO_ENABLED=0
windows: EXE = chef-automate.exe

linux darwin windows: BUILD_TARGET = $@
linux darwin windows: OPTS = GOOS=${BUILD_TARGET} GOARCH=amd64 ${EXTRA_OPTS}

linux darwin windows:
	@echo Building ${EXE} for ${BUILD_TARGET}
	mkdir -p ${CROSS_TARGET_DIR}/${BUILD_TARGET}
	cd ${CROSS_TARGET_DIR}/${BUILD_TARGET}; \
	${OPTS} go build ${GO_LDFLAGS} ${CHEF_AUTOMATE_PKG};
	@echo Done: ${CROSS_TARGET_DIR}/${BUILD_TARGET}

unit:
	@go test --count 1 ./...

verify-serve:
	docker run -it --rm \
	-w "/go/src/github.com/chef/automate/components/automate-cli" \
	-v $$(dirname $$(dirname $$(pwd))):/go/src/github.com/chef/automate \
	-p 7799:7799 \
	cosmtrek/air:v1.43.0 \
	-c /go/src/github.com/chef/automate/components/automate-cli/pkg/verifyserver/rundev/.air.toml
