include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/authn-service

BINS = ${PACKAGE_PATH}/cmd/authn-service ${PACKAGE_PATH}/cmd/migrate-tokens
MIGRATION_READMES = tokens/pg/sql/README.md
VERSION ?= $(shell git rev-pasrse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
GO_LDFLAGS = --ldflags "-X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA}"

packages:=${PACKAGE_PATH}/...
ifdef CI
  verbose?="-v"
endif

all: lint build test
static: lint ${MIGRATION_READMES}
unit: build test

.PHONY: ${MIGRATION_READMES}
${MIGRATION_READMES}:
	../../scripts/generate_and_check_migration_files.sh $@

${BINS}: bin
	@echo "GO $@"
	@cd bin; go build ${GO_LDFLAGS} $@

migrate: bin
	@echo "GO $@"
	@cd bin; go build ${GO_LDFLAGS} ${PACKAGE_PATH}/cmd/migrate-tokens

bin:
	mkdir -p bin

build: ${BINS}

build_prod:
	go build --tags prod ${GO_LDFLAGS} ${PACKAGE_PATH}/cmd/authn-service

mock:
	go build ${PACKAGE_PATH}/cmd/mock

# Note 2017/12/21 (sr): We're passing `-p 1`, and thereby turn off parallel
# test execution, since the clients pg-adapter tests reset the database. If this
# happens while the clients_integration_test are running, those fail in
# seemingly mysterious ways. Since the overall test run still does not take too
# long, this seems acceptable.
test:
	@go test $(verbose) -p 1 $(packages) -cover

test_with_db:
	@docker ps | grep authn-postgres || (echo "Docker postgres not up. Run 'make setup_docker_pg' and try this command again."; exit 1)
	@PG_URL="postgresql://postgres@127.0.0.1:5432/authn_test?sslmode=disable&timezone=UTC" go test -cover -count=1 -parallel=1 -p 1 $(packages)
	@echo "Docker containers still up, run 'make kill_docker_pg' to bring them down or test again with make test_with_db."

setup_docker_pg:
	docker run --name authn-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=authn_test -p 5432:5432 -d postgres:9

kill_docker_pg:
	docker rm -f authn-postgres || true

# Regenerate all *.pb.go files
proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component authn-service'

.PHONY: all fmt test vet static unit
