BINARY=pgleaderchk
PACKAGE_PATH ?= github.com/chef/a2-ha-backend/components/automate-backend-pgleaderchk
PGLEADERCHK_PKG=${PACKAGE_PATH}/cmd/pgleaderchk
BINS = ${PGLEADERCHK_PKG}
GOOS = $(shell go env GOOS)
VERSION ?= 0.1.0
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
LINKER_FLAGS = -X ${PACKAGE_PATH}/pkg/version.Version=${VERSION} -X ${PACKAGE_PATH}/pkg/version.BuildTime=${BUILD_TIME}
GO_LDFLAGS=--ldflags '-extldflags "-static" ${LINKER_FLAGS}'
BIN_DIR ?= bin
CROSS_TARGET_DIR ?= bin

package: linux

# cross compilation targets
cross: linux darwin

build: ${BINS}

${BINS}: ${BIN_DIR} echo-build-data
	@echo "GO $@"
	@cd ${BIN_DIR}; go build ${GO_LDFLAGS} $@

echo-build-data:
	@echo "build_time: ${BUILD_TIME}"
	@echo "   version: ${VERSION}"

${BIN_DIR}:
	mkdir -p ${BIN_DIR}

clean:
	rm -rf bin/*

fmt:
	@echo "go fmt: cmd/... pkg/..."
	@go fmt $(shell go list ./cmd/... ./pkg/...)

LINTERARGS:=--exclude-use-default -D lll ./...

EXE = ${BINARY}
linux: EXTRA_OPTS = CGO_ENABLED=0
darwin: EXTRA_OPTS = CGO_ENABLED=0

linux darwin: BUILD_TARGET = $@
linux darwin: OPTS = GOOS=${BUILD_TARGET} GOARCH=amd64 ${EXTRA_OPTS}

linux darwin:
	@echo Building ${EXE} for ${BUILD_TARGET}
	mkdir -p ${CROSS_TARGET_DIR}/${BUILD_TARGET}
	cd ${CROSS_TARGET_DIR}/${BUILD_TARGET}; \
	${OPTS} go build ${GO_LDFLAGS} ${PGLEADERCHK_PKG};
	@echo Done: ${CROSS_TARGET_DIR}/${BUILD_TARGET}
