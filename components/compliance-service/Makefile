include ../../Makefile.common_go

.PHONY: test

PORT      ?= 10121
SS_PORT   ?= 10131
MANAGER_PORT ?= 10120
EVENT_ENDPOINT ?= 127.0.0.1:10132
SS_CONFIG_PATH ?= /tmp/secrets-service.toml
EVENT_CONFIG_PATH ?= /tmp/event-service.toml
COMPLIANCE_LOG_PATH ?= /tmp/compliance.log
ELASTICSEARCH_PORT   ?= 9200
ELASTICSEARCH_URL     = http://127.0.0.1:$(ELASTICSEARCH_PORT)
ES_SIDECAR_ADDRESS ?= 127.0.0.1:10123
TMP_PATH = /tmp
MARKET_PATH = $(TMP_PATH)/market
PROFILES_PATH = $(TMP_PATH)/profiles
POSTGRES_HOST ?= 127.0.0.1
POSTGRES_PORT ?= 15432
POSTGRES_USER ?= postgres
POSTGRES_PASS ?= admin
POSTGRES_URI  ?= postgresql://$(POSTGRES_USER):$(POSTGRES_PASS)@$(POSTGRES_HOST):$(POSTGRES_PORT)/chef_automate?sslmode=disable
ES_VER ?= 6
PACKAGE = "github.com/chef/automate/components/compliance-service"

GIT_SHA ?= $(shell git rev-parse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
# Add -s -w in LDFLAGS to reduce the size of the binary by ~40%
LDFLAGS="-X $(LIBRARY_PATH)/version.Version=$(BUILD_TIME) -X $(LIBRARY_PATH)/version.GitSHA=$(GIT_SHA) -X $(LIBRARY_PATH)/version.BuildTime=$(BUILD_TIME)"

SECRETS_KEY ?= 12345678901234567890123456789012

MANAGER_SSL_CERT="$(PWD)/../../dev/certs/nodemanager-service.crt"
MANAGER_SSL_KEY="$(PWD)/../../dev/certs/nodemanager-service.key"
SSL_CERT="$(PWD)/../../dev/certs/compliance-service.crt"
SSL_KEY="$(PWD)/../../dev/certs/compliance-service.key"
SS_SSL_CERT="$(PWD)/../../dev/certs/secrets-service.crt"
SS_SSL_KEY="$(PWD)/../../dev/certs/secrets-service.key"
SSL_ROOT_CERT="$(PWD)/../../dev/certs/Chef_Automate_FAKE_Dev.crt"
DEV_CERT_PATHS=--cert $(SSL_CERT) --key $(SSL_KEY) --root-cert $(SSL_ROOT_CERT)
# RUN_MODE=test is used to disable some bits of the code not used during testing now
DEBUG_COMMAND = RUN_MODE="test" dlv debug --headless --listen=:59408 --api-version=2 cmd/compliance-service/main.go -- run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT) --manager-port $(MANAGER_PORT) --event-endpoint $(EVENT_ENDPOINT) --es-sidecar-address $(ES_SIDECAR_ADDRESS) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) --log-level "debug" --config /tmp/.compliance-service.toml
RUN_COMMAND = RUN_MODE="test" go run -ldflags $(LDFLAGS) cmd/compliance-service/main.go run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT) --event-endpoint $(EVENT_ENDPOINT) --es-sidecar-address $(ES_SIDECAR_ADDRESS) --manager-port $(MANAGER_PORT) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) --config /tmp/.compliance-service.toml
RUN_COMMAND_RACECHECK = RUN_MODE="test" go run --race -ldflags $(LDFLAGS) cmd/compliance-service/main.go run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT)  --manager-port $(MANAGER_PORT) --event-endpoint $(EVENT_ENDPOINT) --es-sidecar-address $(ES_SIDECAR_ADDRESS) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) -- log-level "debug" --config /tmp/.compliance-service.toml
RUN_COMMAND_INFO = $(RUN_COMMAND) --log-level "info"
RUN_COMMAND_DEBUG = $(RUN_COMMAND) --log-level "debug"
TEST_PATH = "$(PWD)/api/tests"
RUN_GRPCURL_COMPLIANCE_VERSION = grpcurl --insecure -cert $(SSL_CERT) -key $(SSL_KEY) localhost:$(PORT) chef.automate.domain.compliance.version.VersionService/Version

# Image maintained here: https://github.com/chef/release-engineering/blob/master/components/dockerfiles/chefes/ssh-target/Dockerfile.ubuntu1804
SSH_TARGET_IMAGE?=chefes/ssh-target-ubuntu1804:latest

GOPATH = $(shell go env GOPATH)

# Replaces 'unknown' terminal with 'xterm' for 'tput' colors to work in buildkite
TERM := $(subst unknown,xterm,$(TERM))

# https://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux
red=`tput setaf 1`
green=`tput setaf 2`
yellow=`tput setaf 3`
blue=`tput setaf 6`
reset=`tput sgr0`

## BUILD ##
build: build-linux build-windows build-mac
	@printf "\n===> ${blue}Building all packages...${reset}\n"

build-linux:
	# to ensure this build works in alpine
	# https://stackoverflow.com/questions/36279253/go-compiled-binary-wont-run-in-an-alpine-docker-container-on-ubuntu-host
	@printf "\n===> ${blue}Building linux package...${reset}\n"
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -tags netgo -ldflags $(LDFLAGS) -o dist/linux/compliance cmd/compliance-service/main.go

build-windows:
	@printf "\n===> ${blue}Building windows package...${reset}\n"
	GOOS=windows GOARCH=amd64 go build -ldflags $(LDFLAGS) -o dist/win/compliance.exe cmd/compliance-service/main.go

build-mac:
	@printf "\n===> ${blue}Building mac package...${reset}\n"
	GOOS=darwin GOARCH=amd64 go build -ldflags $(LDFLAGS) -o dist/mac/compliance cmd/compliance-service/main.go

## TOOLS ##
ruby-grpc-tools:
	@printf "\n===> ${blue}Installing grpc gems...${reset}\n"
	@if ! gem list grpc -i; then gem install grpc -v 1.28.0 ; fi > /dev/null
	@if ! gem list grpc-tools -i; then gem install grpc-tools -v 1.28.0; fi > /dev/null

PROTOS = $(shell find ../../api/interservice/compliance ../../api/external/common ../../api/external/annotations -name '*.proto')
RUBY_PROTOS = $(patsubst %.proto, %_pb.rb, $(PROTOS))

generate-ruby-grpc:
	@printf "\n===> ${blue}Generating ruby grpc files${reset}\n"
	$(MAKE) ruby-grpc-tools $(RUBY_PROTOS)
	$(MAKE) generate-ruby-grpc-nodemanager
	$(MAKE) generate-ruby-grpc-secrets-service

generate-ruby-grpc-nodemanager:
	cd ../nodemanager-service && make generate-ruby-grpc

generate-ruby-grpc-secrets-service:
	cd ../secrets-service && make generate-ruby-grpc

%_pb.rb : %.proto
	@printf " * ${blue}Compiling test proto${reset} for $<\n"
	@-grpc_tools_ruby_protoc \
		-I../../api \
		-I../../components \
		-I../../protovendor \
		-I../../protovendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
		--ruby_out=. \
		--grpc_out=. \
		$<

proto:
	@printf "\n===> ${blue}Generating compliance-service protos${reset}\n"
	cd ../../ && hab studio run 'source .studiorc; compile_all_protobuf_components'

gateway-proto:
	@printf "\n===> ${blue}Generating automate-gateway protos${reset}\n"
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component automate-gateway'

wait-for-compliance-service:
	@printf "\n===> ${blue}Waiting for the compliance service to be ready...${reset}\n"; \
	i=1; \
	max_loops=50; \
	printf "\n===> ${blue}Checking Compliance Service version to ensure it's ready to receive connections...${reset}\n"; \
	$(RUN_GRPCURL_COMPLIANCE_VERSION); \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i Compliance Service is not running, retrying...${reset}\n"; \
	  sleep 5; \
	  i=$$(( $$i + 1 )); \
	  $(RUN_GRPCURL_COMPLIANCE_VERSION); \
	done; \
	if [ $$i -eq $$max_loops ]; then \
	  printf "${red}Giving up checking for Compliance Service version${reset}\n"; \
	  exit 1; \
	fi; \
	printf " * ${green}Compliance Service is running, moving on...${reset}\n"


## RUN COMMANDS ##
run-log-level-info: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	$(MAKE) run-nodemanager
	@printf "\n===> ${blue}Running compliance-service in background${reset} with logs redirected to ${yellow}$(COMPLIANCE_LOG_PATH)${reset}\n"
	$(RUN_COMMAND_INFO) > $(COMPLIANCE_LOG_PATH) 2>&1 &
	# since we background the service, we need to give it some time to start
	@$(MAKE) wait-for-compliance-service

run: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	@$(MAKE) golang_version_check
	$(MAKE) run-nodemanager
	@printf "\n===> ${blue}Running compliance-service in background${reset} with logs redirected to ${yellow}$(COMPLIANCE_LOG_PATH)${reset}\n"
	$(RUN_COMMAND_DEBUG) > $(COMPLIANCE_LOG_PATH) 2>&1 &
	# since we background the service, we need to give it some time to start
	@$(MAKE) wait-for-compliance-service

run-debug: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	@$(MAKE) golang_version_check
	$(MAKE) run-nodemanager
	@printf "\n===> ${blue}Running compliance-service${reset}\n"
	$(DEBUG_COMMAND)

run-with-logs: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	$(MAKE) run-nodemanager
	@printf "\n===> ${blue}Running compliance-service${reset}\n"
	$(RUN_COMMAND_DEBUG)
	@$(MAKE) wait-for-compliance-service

run-with-race-check: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	$(MAKE) run-nodemanager
	$(RUN_COMMAND_RACECHECK)
	@$(MAKE) wait-for-compliance-service

run-with-es-pg:
	$(MAKE) start-es-pg
	$(MAKE) run

run-nodemanager:
	@printf "\n===> ${blue}Starting nodemanager and dependent services...${reset}\n"
	@cd ../nodemanager-service && make run

clean:
	@printf "\n===> ${yellow}Clearing compliance and dependent services...${reset}\n"
	@# If we don't match he process with /(tmp|var) at the beginning, in pkill will fail itself and fail the make.
	@pkill -x -f "/(tmp|var).+ run --port $(PORT) .*" || true
	@cd ../nodemanager-service && make clean


## ELASTICSEARCH, POSTGRES, SSH NODE ##
start-es-pg: start-pg create-es-data-dir
	@printf "\n===> ${blue}Running start-es-pg...${reset}\n"; \
	data_dir="$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)"; \
	if [ "$(A1_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test a1 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es5-a1-data.tar.gz -C "$$data_dir"; \
	  printf "===> ${yellow}Loading test a1 data into Postgres${reset}...\n"; \
	  docker cp test_data/es-pg/a1-pg-dump.sql.gz cc_pg:/tmp/; \
	  docker exec cc_pg rm -f /tmp/a1-pg-dump.sql; \
	  docker exec cc_pg gunzip /tmp/a1-pg-dump.sql.gz; \
	  docker exec cc_pg psql -Upostgres chef_automate -f /tmp/a1-pg-dump.sql; \
	fi; \
	if [ "$(A2V1_DATA)" = "true" ]; then \
		printf "\n===> ${yellow}Loading test A2v1 data into ElasticSearch${reset} ($$data_dir)...\n"; \
		tar -xzvf test_data/es-pg/es6-a2v1-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V2_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v2 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es6-a2v2-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V3_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v3 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es6-a2v3-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V4_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v4 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es6-a2v4-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V5_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v5 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es6-a2v5-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V6_DATA)" = "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v6 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf test_data/es-pg/es6-a2v6-data.tar.gz -C "$$data_dir"; \
	fi; \
	chmod -R 775 "$$data_dir"; \
	my_primary_group=`id -g -n $(USER)`; \
	chown -R $$USER:$$my_primary_group "$$data_dir"; \
	printf "\n * ${blue}ElasticSearch data dir ($$data_dir) contents:${reset}\n"; \
	ls -la "$$data_dir";
	@$(MAKE) start-es

start-es: start-es$(ES_VER)

start-es5: create-es-data-dir
	@printf "\n===> ${yellow}Recreating the ElasticSearch 5 container...${reset}\n"
	docker rm -f cc_es5 2> /dev/null; sleep 1; docker run -d -v "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)":/usr/share/elasticsearch/data -v "$(PWD)/elasticsearch/config/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" --name cc_es5 -p $(ELASTICSEARCH_PORT):9200 -p 9305:9300 elasticsearch:5.4.0
	@$(MAKE) wait-for-es

start-es6: create-es-data-dir
	@printf "\n===> ${yellow}Recreating the ElasticSearch 6 container...${reset}\n"
	docker rm -f cc_es6 2> /dev/null; sleep 1; docker run -d -v "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)":/usr/share/elasticsearch/data -v "$(PWD)/elasticsearch/config/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" --name cc_es6 -p $(ELASTICSEARCH_PORT):9200 -p 9305:9300 -e "xpack.security.enabled=false" docker.elastic.co/elasticsearch/elasticsearch:6.2.2
	@$(MAKE) wait-for-es

create-es-data-dir:
	@printf "\n===> ${blue}Creating the ES data directory...${reset}\n"
	@mkdir -p "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)" && chmod 777 "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)"

purge-es-data:
	@printf "\n===> ${yellow}Purging ES data...${reset}\n"
	@rm -rf "$(PWD)/elasticsearch/.tmp"

# for automate 2.0, we'll be on pg6
start-pg:
	@printf "\n===> ${yellow}Recreating the PostgreSQL container...${reset}\n"
	docker rm -f cc_pg 2> /dev/null; sleep 1; docker run --name cc_pg -e POSTGRES_DB=chef_automate -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=admin -p $(POSTGRES_PORT):5432 -d postgres:9.6.11 || docker start cc_pg
	@$(MAKE) wait-for-pg
	@printf "\n * ${blue}Creating the PostgreSQL databases...${reset}\n"
	docker exec -it cc_pg psql -Upostgres -c 'CREATE DATABASE secrets_service;'
	docker exec -it cc_pg psql -Upostgres -c 'CREATE DATABASE nodemanager_service;'

# Login to postgres and type away: \d, select * from nodes;, etc
login-pg:
	@printf "\n===> ${blue}Logging in to postgres...${reset}\n"
	docker exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" -it cc_pg psql -Upostgres chef_automate

clear-es-pg: clear-es clear-pg

# Empty the postgres testing, useful for dev&test
clear-pg:
	docker exec -it cc_pg psql -Upostgres chef_automate -c 'TRUNCATE nodes, node_managers, tags, agents, s_secrets, results, jobs, profiles CASCADE;'

clear-es: clear-es-indices clear-es-templates

clear-es-indices:
	@printf "\n===> ${blue}Clearing ES indices...${reset}\n"
	curl -X DELETE "$(ELASTICSEARCH_URL)/comp-*"

clear-es-templates:
	@printf "\n===> ${blue}Clearing ES templates...${reset}\n"
	curl -X DELETE "$(ELASTICSEARCH_URL)/_template/comp-*"

clean-all: clean
	@printf "\n===> ${yellow}Clearing containers...${reset}\n"
	@docker rm -f cc_pg cc_es5 cc_es6 cc_ssh_node 2> /dev/null || true
	@$(MAKE) purge-es-data
	@printf "\n===> ${yellow}Clearing profiles directories...${reset}\n"
	@rm -rf $(PROFILES_PATH)
	@rm -rf $(MARKET_PATH)

start-ssh-node:
	@printf "\n===> ${blue}Ensuring we have a cc_ssh_node container${reset} to be used as ssh node target\n"
	docker rm -f cc_ssh_node 2> /dev/null || true
	docker pull $(SSH_TARGET_IMAGE)
	docker run -p 11030:22 --name cc_ssh_node -d $(SSH_TARGET_IMAGE)

# need to use this here: pg_isready
# and change database to chef_compliance_service
wait-for-pg:
	@printf "\n===> ${blue}Waiting for postgres to be ready...${reset}\n"; \
	i=1; \
	max_loops=20; \
	printf "\n===> ${blue}Checking PostgreSQL to ensure it's ready to receive connections${reset}\n"; \
	docker exec -it cc_pg psql -Upostgres chef_automate -c "\d" > /dev/null; \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i PostgreSQL is not healthy yet, retrying...${reset}\n"; \
	  sleep 2; \
	  i=$$(( $$i + 1 )); \
	  docker exec -it cc_pg psql -Upostgres chef_automate -c "\d" > /dev/null; \
	done; \
	if [ $$i -eq $$max_loops ]; then \
		printf "${red}Giving up, cc_pg logs:${reset}\n"; \
		docker logs cc_pg --since 2000; \
		exit 1; \
	fi; \
	printf " * ${green}PostgreSQL is healthy, moving on...${reset}\n"

wait-for-es:
	@printf "\n===> ${blue}Checking ElasticSearch to ensure it's ready to receive connections${reset}\n"; \
	i=1; \
	max_loops=100; \
	curl -s -X GET "$(ELASTICSEARCH_URL)/_cluster/health" | grep -E '"status":"yellow"|"green"'; \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i ElasticSearch is not healthy yet, retrying...${reset}\n"; \
	  sleep 5; \
	  i=$$(( $$i + 1 )); \
	  curl -s -X GET "$(ELASTICSEARCH_URL)/_cluster/health" | grep -E '"status":"yellow"|"green"'; \
	done; \
	if [ $$i -eq $$max_loops ]; then \
	  curl -s -X GET "$(ELASTICSEARCH_URL)/_cluster/health"; \
		curl -s -X GET "$(ELASTICSEARCH_URL)/_cat/indices?s=i"; \
		printf "${red}Giving up, cc_es$(ES_VER) logs:${reset}\n"; \
		docker logs cc_es$(ES_VER) --since 2000; \
		exit 1; \
	fi; \
	printf "\n * ${blue}ElasticSearch indices:${reset}\n"; \
	curl -s -X GET "$(ELASTICSEARCH_URL)/_cat/indices?v&s=i"; \
	printf " * ${green}ElasticSearch is healthy, moving on...${reset}\n\n";

# Waiting to avoid starting the tests before the test reports have been ingested 100%
wait-for-ingestion:
	@printf "\n===> ${blue}Waiting for ingestion to complete...${reset}\n"; \
	i=1; \
	curl_output="-=init=-"; \
	max_loops=50; \
	es_docs=45; \
	healthy_match="\"total\":$$es_docs,\"successful\":$$es_docs"; \
	printf "\n===> ${blue}Checking ElasticSearch to ensure it has ingested our reports${reset}\n"; \
	curl -s -X GET "$(ELASTICSEARCH_URL)/comp-*-r-20*/_doc/_count" | grep $$healthy_match > /dev/null; \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i ElasticSearch is still ingesting, retrying...${reset}\n"; \
	  sleep 5; \
	  i=$$(( $$i + 1 )); \
	  curl_output=`curl -s -X GET "$(ELASTICSEARCH_URL)/comp-*-r-20*/_doc/_count"`; \
		echo $$curl_output | grep $$healthy_match > /dev/null; \
	done; \
	if [ $$i -eq $$max_loops ]; then \
	  printf "\n${red}Timeout waiting for ingestion to complete, _doc/_count output:${yellow}\n$$curl_output\n${reset}doesn't include:\n${green}$$healthy_match${reset}\n\ncc_es$(ES_VER) logs:\n"; \
	  docker logs cc_es$(ES_VER) --since 2000; \
	  exit 1; \
	fi; \
	ls -la "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)";
	@printf " * ${green}ElasticSearch finished successfully ingesting our reports!${reset}\n";

## Use this if you don't want to worry about the order in which to start things when testing.. it does it all!
run-test: clean-all test-prep run ingest-reports-into-es test

## TESTING ##
test-db: run-with-es-pg run-db-tests

run-db-tests:
	@printf "\n===> ${blue}Running db tests...${reset}\n"
	@POSTGRES_URI="$(POSTGRES_URI)" CGO_ENABLED=0 \
		go test -v ./dao/... ./profiles/db ./scanner/... ./inspec-agent/scheduler -cover -database -parallel=1 -p 1 -failfast

test-integration: start-es start-pg
	@printf "\n===> ${blue}Running integration tests with ES_VER=$(ES_VER)${reset}\n"
	@$(MAKE) start-ssh-node
	@$(MAKE) download-sample-market-profiles
	@$(MAKE) clear-es
	# we run the service locally so we can inspec detect and exec containers and ssh into containers
	$(MAKE) run-log-level-info
	@if $(MAKE) test-reporting test-scanner ; then \
	  printf "===> ${green}integration tests passed!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}integration tests failed!!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-integration-go:
	@printf "\n===> ${blue}These tests depend on a few things that can be provisioned with: ${green}make clean-all start-es start-pg run${reset}\n"; \
  printf "\n===> ${blue}Running all go integration tests. To run just one, do it like this: ${green}TEST=TestReadFailures make test-integration-go${reset}\n\n"; \
	GO_TEST_CMD='go test -v "github.com/chef/automate/components/compliance-service/integration_test"'; \
	if [ -n "$$TEST" ]; then GO_TEST_CMD="$$GO_TEST_CMD -run $(TEST)"; fi; \
	if ELASTICSEARCH_URL='http://localhost:9200' eval $$GO_TEST_CMD ; then \
	  printf "===> ${green}go integration tests passed!!!${reset}\n"; \
	else \
	  printf "===> ${red}go integration tests failed!!!${reset}\n"; \
	  exit 1; \
	fi;

test-integration-reporting: start-es start-pg download-sample-market-profiles clear-es run-log-level-info
	@printf "\n===> ${blue}Running reporting integration tests with ES_VER=$(ES_VER)${reset}\n"
	# we run the service locally so we can inspec detect and exec containers and ssh into containers
	@if $(MAKE) test-reporting ; then \
	  printf "===> ${green}reporting integration tests passed!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}reporting integration tests failed!!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-integration-scanner: start-es start-pg start-ssh-node download-sample-market-profiles run-log-level-info
	@printf "\n===> ${blue}Running integration tests with PG6 and ES_VER=$(ES_VER)${reset}\n"
	# we run the service locally so we can inspec detect and exec containers and ssh into containers
	@if $(MAKE) test-scanner ; then \
	  printf "===> ${green}scanner integration tests passed!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}scanner integration tests failed!!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-automate-upgrade: clean-all
	printf "\n===> Running Automate upgrade integration tests\n"
	@if [ "$(A1_DATA)" != "true" ] && [ "$(A2V1_DATA)" != "true" ] && [ "$(A2V2_DATA)" != "true" ] && [ "$(A2V3_DATA)" != "true" ] && [ "$(A2V4_DATA)" != "true" ] && [ "$(A2V5_DATA)" != "true" ] && [ "$(A2V6_DATA)" != "true" ]; then \
	  printf "===> ${red}Aborting test-automate-upgrade as one of these ENV variables is not true: A1_DATA, A2V1_DATA, A2V2_DATA, A2V3_DATA, A2V4_DATA, A2V5_DATA, A2V6_DATA${reset}\n"; \
	  exit 2; \
	fi;
	@if printf "$(TEST)" | grep -i -q "^6.*"; then \
	  printf "\n===> Using integration TEST=$(TEST)\n"; \
	else \
	  printf "===> ${red}Aborting test-automate-upgrade as TEST ENV variable must start with 6, for example: 61_A2V1_migration_spec.rb${reset}\n"; \
	  exit 3; \
	fi;
	@$(MAKE) start-es-pg;
	@$(MAKE) run;
	# Give a few seconds for the background migrations to finish
	sleep 20;
	@if $(MAKE) test ; then \
	  printf "===> ${green}Automate upgrade integration tests passed!!!${reset}\n"; \
	else \
	  printf "===> ${red}Automate upgrade integration tests failed!!!${reset}\n"; \
	  exit 1; \
	fi;

test-ingest-to-manager-conn:
	@printf "\n===> ${blue}Testing ingestion to manager...${reset}\n"
	go run examples/ingest/ingest_client.go --file ingest/examples/compliance-success-tiny-report.json
	go run examples/ingest/ingest_client.go --file ingest/examples/compliance-failure-big-report.json
	# now send the first report in again, to ensure we get two, not three nodes
	go run examples/ingest/ingest_client.go --file ingest/examples/compliance-success-tiny-report.json
	$(MAKE) test TEST="test_ingest_to_mgr*"

test-reporting: ingest-reports-into-es
	@printf "\n===> ${blue}Running the reporting integration tests${reset}\n"
	# the service sometimes isn't ready in travis if we don't sleep 15
	sleep 15
	TEST='[0-1]*_spec.rb' $(MAKE) test
	cd api/tests && CGO_ENABLED=0 go test export_test.go -failfast
	$(MAKE) test-ingest-to-manager-conn

test-scanner:
	@printf "\n===> ${blue}Running the scanner integration tests${reset}\n"
	TEST='[2-5]*_spec.rb' $(MAKE) --debug=v test

# Run individual tests with: TEST="02_nodes_spec.rb" make test
# or multiple ones with: TEST="02_*" make test
test: generate-ruby-grpc wait-for-compliance-service
	@cd api/tests; \
	pwd; \
	printf "\n===> ${blue}Preparing to run tests...${reset}\n"; \
	if [ -z "$$TEST" ]; then TEST='*_spec.rb'; fi; \
	matching_tests=`ls -1q $$TEST 2> /dev/null | wc -l`; \
	if [ $$matching_tests -eq 0 ]; then \
		printf "\n${red}ERROR: TEST pattern $$TEST has not match, aborting...${reset}\n"; \
		exit 17; \
	else \
	  @printf "\n===> ${blue}Running $$matching_tests test files...${reset}\n"; \
	fi; \
	bundler_version=`grep -A 1 "BUNDLED WITH" api/tests/Gemfile.lock | tail -1 | tr -d '[:space:]'`; \
	gem install bundler -f --version "= $$bundler_version"; \
	printf "\n===> ${blue}Running 'bundle install' in api/tests...${reset}\n"; \
	pwd; \
	bundle install; \
	printf "\n===> ${blue}Running the tests via run.rb...${reset}\n"; \
	PORT=$(PORT) \
	SS_PORT=$(SS_PORT) \
	MANAGER_PORT=$(MANAGER_PORT) \
	SSL_CERT=$(SSL_CERT) \
	SSL_KEY=$(SSL_KEY) \
	SS_SSL_CERT=$(SS_SSL_CERT) \
	SS_SSL_KEY=$(SS_SSL_KEY) \
	MANAGER_SSL_CERT=$(MANAGER_SSL_CERT) \
	MANAGER_SSL_KEY=$(MANAGER_SSL_KEY) \
	SSL_ROOT_CERT=$(SSL_ROOT_CERT) \
	MARKET_PATH=$(MARKET_PATH) \
	PROFILES_PATH=$(PROFILES_PATH) \
	bundle exec ./run.rb $$TEST; \
	echo

test-prep: $(MARKET_PATH) $(PROFILES_PATH) start-ssh-node download-sample-market-profiles install-inspec start-es-pg

install-inspec:
	@printf "\n===> ${blue}Installing inspec if needed...${reset}\n"
	@inspec --version 2> /dev/null | grep -q $(shell cat ../../INSPEC_VERSION) || gem install inspec-bin --no-document --version $(shell cat ../../INSPEC_VERSION)

test-examples:
	@printf "===> REQUIRES RUNNING SERVICE\n"
	$(MAKE) send-ingest-report
	cd examples/reporting/test && go test . -failfast
	cd examples/profiles/test && go test . -failfast

test-polling: start-ssh-node
	$(MAKE) test TEST="poll_test*"

test-unit:
	@printf "\n ===> ${blue}Running compliance-service go unit-tests...${reset}\n"
	@go test $(shell GOFLAGS='$(GOFLAGS)' go list ./... |  grep -v '^github.com/chef/automate/components/compliance-service/\(examples\|feed/\|api/automate-feed\|api/automate-event\|api/tests\|scanner\|integration_test\)') -cover

send-ingest-report:
	@printf "\n===> ${blue}Sending report to ingestion endpoint...${reset}\n"
	go run examples/ingest/ingest_client.go --file ingest/examples/compliance-success-tiny-report.json --threads 1 --reports-per-thread 1

## DATA ##
$(MARKET_PATH):
	@printf "\n===> ${blue}Creating profiles market directory...${reset}\n"
	@mkdir -p $(MARKET_PATH)

$(PROFILES_PATH):
	@printf "\n===> ${blue}Creating profiles directory...${reset}\n"
	@mkdir -p $(PROFILES_PATH)

download-sample-market-profiles: $(MARKET_PATH)
	@printf "\n===> ${blue}Downloading profiles...${reset}\n"
	@if [ -f $(MARKET_PATH)/linux-patch-baseline-0.3.0.tar.gz ]; then printf " * ${blue}linux-patch-baseline${reset} already downloaded\n"; else curl -LSs -o $(MARKET_PATH)/linux-patch-baseline-0.3.0.tar.gz https://github.com/dev-sec/linux-patch-baseline/archive/0.3.0.tar.gz; fi;
	@if [ -f $(MARKET_PATH)/windows-baseline-1.1.0.tar.gz ]; then printf " * ${blue}windows-baseline${reset} already downloaded\n"; else curl -LSs -o $(MARKET_PATH)/windows-baseline-1.1.0.tar.gz https://github.com/dev-sec/windows-baseline/archive/1.1.0.tar.gz; fi;

	@printf "\n===> ${blue}Generating inspec jsons for $(MARKET_PATH)${reset} profiles...\n"
	@FILES=$$(ls -1 $(MARKET_PATH) | grep '\.tar.gz$$'); \
	for file in $$FILES; do \
	  file="$(MARKET_PATH)/$$file"; \
	  if [ -f "$$file.json" ]; then \
	    printf " * ${blue}$$file.json${reset} already generated, skipping...\n"; \
	  else \
	    printf " * Creating ${blue}$$file.json${reset}\n"; \
	    inspec json "$$file" > "$$file.json" --chef-license="accept-no-persist"; \
	  fi; \
	done; \
	echo;

	@# we use this to test zip upload
	@if [ -f $(TMP_PATH)/ssl-baseline-1.3.0.zip ]; then printf " * ${blue}$(TMP_PATH)/ssl-baseline-1.3.0.zip${reset} already downloaded\n"; else curl -LSs -o $(TMP_PATH)/ssl-baseline-1.3.0.zip https://github.com/dev-sec/ssl-baseline/archive/1.3.0.zip; fi;
	@if [ -f $(TMP_PATH)/ssl-baseline-1.4.0.zip ]; then printf " * ${blue}$(TMP_PATH)ssl-baseline-1.4.0.zip${reset} already downloaded\n"; else curl -LSs -o $(TMP_PATH)/ssl-baseline-1.4.0.zip https://github.com/dev-sec/ssl-baseline/archive/1.4.0.zip; fi;
	cp -f $(TEST_PATH)/mario-0.1.0.tar.gz $(TMP_PATH)

# when running against local es this requires start-es
# when running against es that is inside vm (virtualbox)  be sure to edit /opt/delivery/sv/elasticsearch/run and change -Des.network.host=127.0.0.1 to -Des.network.host=0.0.0.0
# in order to tunnel to a remote ES, we must trust the certificate that the remote instance is offering.  get this from your browser and add it to the keychain on the respective OS.
ingest-reports-into-es:
	@printf "\n===> ${blue}Ingesting reports into ES...${reset}\n"
	@if  [ "$(A1_DATA)" != "true" ] && [ "$(A2V1_DATA)" != "true" ] && [ "$(A2V2_DATA)" != "true" ] && [ "$(A2V3_DATA)" != "true" ] && [ "$(A2V4_DATA)" != "true" ] && [ "$(A2V5_DATA)" != "true" ] && [ "$(A2V6_DATA)" != "true" ]; then \
	  printf "\n===> PROCESSING REPORTS FROM ${blue}test_data/audit_reports${reset}\n"; \
	  FILES=$$(ls -1 test_data/audit_reports | grep '\.json'); \
	  for file in $$FILES; do \
	    file="test_data/audit_reports/$$file"; \
	    printf "\n * Ingesting report: ${blue}$$file${reset}\n"; \
	    go run examples/ingest/ingest_client.go --file "$$file"; \
	  done; \
	  $(MAKE) wait-for-ingestion; \
	fi;

INSPEC_RUNNER_PATH = $(GOPATH)/bin/inspec_runner

build-inspec-runner: install-inspec $(INSPEC_RUNNER_PATH)

$(INSPEC_RUNNER_PATH): $(shell find cmd/inspec_runner)
	go build --ldflags "-X main.EXECUTABLE_PATH=$(shell which inspec)" -o $(INSPEC_RUNNER_PATH) github.com/chef/automate/components/compliance-service/cmd/inspec_runner
	@# Go does not rebuild files that are up to date, so manually update the timestamp of the binary
	sudo touch $(INSPEC_RUNNER_PATH)
	sudo chown root: $(INSPEC_RUNNER_PATH)
	sudo chmod u+s $(INSPEC_RUNNER_PATH)

logs:
	@tail -f /tmp/compliance.log | grep -v -E "(Worker s|dead tasks|Recurring workflow|Cleaning workflow results table)"
