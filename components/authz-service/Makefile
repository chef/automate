include ../../Makefile.common_go

PACKAGE_PATH = github.com/chef/automate/components/authz-service

BINS = ${PACKAGE_PATH}/cmd/authz-service
MIGRATION_READMES = storage/postgres/migration/sql/README.md storage/postgres/datamigration/sql/README.md
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
	../../scripts/generate_and_check_migration_files $@

${BINS}: bin
	@echo "GO $@"
	@cd bin; go build --race ${GO_LDFLAGS} $@

bin:
	mkdir -p bin

build: ${BINS}

test:
	@go test $(verbose) -cover -count=1 -parallel=1 -p 1 $(packages)

PG_URL ?= "postgresql://postgres@127.0.0.1:5432/authz_test?sslmode=disable"

test_with_db: db_container
	@PGTZ=UTC PG_URL=$(PG_URL) go test -cover -count=1 -parallel=1 -p 1 $(packages)
	@echo "Docker containers still up, run 'make kill_docker_pg' to bring them down or test again with make test_with_db."

test_properties_with_db: db_container
	@PGTZ=UTC PG_URL=$(PG_URL) go test -cover -count=1 -parallel=1 -p 1 $(packages) -run Properties
	@echo "Docker containers still up, run 'make kill_docker_pg' to bring them down or test again with make test_with_db."

.PHONY: db_container
db_container:
	@docker ps | grep authz-postgres || (echo "Docker postgres not up. Run make setup_docker_pg and try this command again."; exit 1)
	psql -d $(PG_URL) -c "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

setup_docker_pg:
	docker run --name authz-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=authz_test -p 5432:5432 -d postgres:9


kill_docker_pg:
	docker rm -f authz-postgres || true

# Regenerates OPA data from rego files
HAVE_GO_BINDATA := $(shell command -v go-bindata 2> /dev/null)
generate:
ifndef HAVE_GO_BINDATA
	@echo "requires 'go-bindata' (go get -u github.com/kevinburke/go-bindata/go-bindata)"
	@exit 1 # fail
else
	go generate ./...
endif

# Regenerate all *.pb.go files
proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component authz-service'

.PHONY: all static unit build compile test lint
