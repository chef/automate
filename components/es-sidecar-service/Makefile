include ../../Makefile.common_go
LINTERARGS=-D lll ./... ../../api/config/es-sidecar-service/...
PROJECT=es-sidecar-service
ORG_PATH=github.com/chef/automate/components
PACKAGE_PATH = $(ORG_PATH)/es-sidecar-service
REPO_PATH=$(ORG_PATH)/$(PROJECT)

GIT_SHA = $(shell git rev-parse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
VERSION = ${BUILD_TIME}
GO_LDFLAGS = --ldflags "-X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA} -X ${LIBRARY_PATH}/version.BuildTime=${BUILD_TIME}"

ELASTICSEARCH_PORT   ?= 9200
ELASTICSEARCH_URL     = http://127.0.0.1:$(ELASTICSEARCH_PORT)

GOPATH = $(shell go env GOPATH)

default: build

ci: lint unit integration

echo-build-data:
	@echo "git_sha: ${GIT_SHA}"
	@echo "version: ${VERSION}"

build: echo-build-data
	@echo build ${GO_LDFLAGS} cmd/es-sidecar-service/es-sidecar-service.go
	@go build ${GO_LDFLAGS} cmd/es-sidecar-service/es-sidecar-service.go

run:
	@go run cmd/es-sidecar-service/es-sidecar-service.go --config ./examples/config.dev.toml serve

habitat-build:
	hab pkg build .

unit:
	go test -v $(shell go list ./... | grep -v '/vendor/' | grep -v '/integration_test') -cover

integration:
	cd ../..; hab studio -D $(STUDIO_OPTS) run "source .studiorc; es_sidecar_service_integration"

vet:
	@go vet $(shell go list ./... | grep -v '/vendor/')

purge-es6:
	rm -rf "$(PWD)/.tmp/es-sidecar-service-es6-data"
start-es6:
	docker rm -f es-sidecar-service-es6 || true
	docker run -d -v "$(PWD)/.tmp/es-sidecar-service-es6-data":/usr/share/elasticsearch/data -v "$(PWD)/test/elastic/config/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" --name es-sidecar-service-es6 -p $(ELASTICSEARCH_PORT):9200 -p 9305:9300 docker.elastic.co/elasticsearch/elasticsearch-oss:6.2.2
