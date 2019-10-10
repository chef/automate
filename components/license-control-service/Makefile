default: ci

ci: build

include ../../Makefile.common_go

review: lint

# Utilities
build:
	go build cmd/license-control-service/license-control-service.go

clean:
	@echo "Removing artifacts..."
	@[[ -f license-control-service ]] && rm license-control-service

generate:
	go generate ./...

setup_docker_pg:
	docker run --name lcs-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=lcs_test -p 5432:5432 -d postgres:9

kill_docker_pg:
	docker rm -f lcs-postgres || true

PG_URL ?= "postgresql://postgres@127.0.0.1:5432/lcs_test?sslmode=disable"
test:
	PG_URL=$(PG_URL) go test -cover ./...

run:
	go run cmd/license-control-service/license-control-service.go serve --config config.dev.toml

.PHONY: ci review clean generate test run
