REPOROOT=../
include ../Makefile.common_go

unit:
	go test -v ./...


PG_URL ?= "postgresql://postgres@127.0.0.1:5432/cereal_test?sslmode=disable&timezone=UTC"
PG_ADMIN_URL ?= "postgresql://postgres@127.0.0.1:5432/template1?sslmode=disable&timezone=UTC"

cereal_integration: cereal_db_container
	@PG_URL=$(PG_URL) PG_ADMIN_URL=$(PG_ADMIN_URL) go test -parallel=1 -count=1 -tags integration ./cereal/integration/...

.PHONY: cereal_db_container
cereal_db_container:
	@docker ps | grep cereal-postgres || (echo "Docker postgres not up. Run make setup_docker_pg and try this command again."; exit 1)

setup_docker_pg:
	docker run --name cereal-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=cereal_test -p 5432:5432 -d postgres:9

kill_docker_pg:
	docker rm -f cereal-postgres || true
