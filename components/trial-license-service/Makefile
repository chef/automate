include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/trial-license-service
BINS = ${PACKAGE_PATH}/cmd/trial-license-service
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
	@go test -v -i $(packages)
	@go test -v -cover -count=1 $(packages)