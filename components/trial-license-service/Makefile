include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/trial-license-service
BINS = ${PACKAGE_PATH}/cmd/trial-license-service
packages:=${PACKAGE_PATH}/cmd/trial-license-service/...

all: lint build test

static: lint
unit: build test

${BINS}: bin
    @echo "GO $@"
    @go build -o bin/trial-license-service ${GO_LDFLAGS} ${PACKAGE_PATH}/cmd/trial-license-service

bin:
    mkdir -p bin

build: ${BINS}

test:
    @go test -v -cover -count=1 $(packages)
	