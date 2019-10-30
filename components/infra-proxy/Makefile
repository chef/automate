include ../../Makefile.common_go

build:
	mkdir -p bin/
	go build -mod=vendor -o bin/infra-proxy ./cmd/infra-proxy

setup_docker_pg:
	docker run --name infra-proxy-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=infra_proxy_test -p 5432:5432 -d postgres:9
	sleep 5 # let docker come up
	# This creates the extension we need to use UUIDs in the migrations.
	# Done in habitat in prod. Not done in code because you must be a superuser.
	docker exec infra-proxy-postgres psql -d "postgresql://postgres@127.0.0.1:5432/infra_proxy_test?sslmode=disable" -c "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

kill_docker_pg:
	docker rm -f -postgres || true
