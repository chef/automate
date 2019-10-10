REPOROOT=../
include ../Makefile.common_go

unit:
	go test -v ./...


PG_URL ?= "postgresql://postgres@127.0.0.1:5432/cereal_test?sslmode=disable&timezone=UTC"
PG_ADMIN_URL ?= "postgresql://postgres@127.0.0.1:5432/template1?sslmode=disable&timezone=UTC"

cereal_integration:
	@PG_URL=$(PG_URL) PG_ADMIN_URL=$(PG_ADMIN_URL) go test -mod=vendor -p 1 -v -parallel=1 -count=1 -tags integration ./cereal/integration/... ./cereal/postgres/...

setup_docker_pg:
	docker run --name cereal-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=cereal_test -p 5432:5432 -d postgres:9

kill_docker_pg:
	docker rm -f cereal-postgres || true
