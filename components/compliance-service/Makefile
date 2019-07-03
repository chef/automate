# lll: ignore long lines
# unparam: unused parameters
# nakedret: naked returns in long functions
# prealloc: slices that could be preallocated
LINTERARGS=-v -D lll -D unparam -D nakedret -D prealloc --deadline 1m ./...
# govet in 1.16 is very confused by some of the code in this component
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
DEBUG_COMMAND = RUN_MODE="test" dlv debug --headless --listen=:59408 --api-version=2 cmd/compliance-service/main.go -- run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT) --manager-port $(MANAGER_PORT) --event-endpoint $(EVENT_ENDPOINT) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) --log-level "debug" --config /tmp/.compliance-service.toml
RUN_COMMAND = RUN_MODE="test" go run -ldflags $(LDFLAGS) cmd/compliance-service/main.go run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT)  --event-endpoint $(EVENT_ENDPOINT)  --manager-port $(MANAGER_PORT) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) --config /tmp/.compliance-service.toml
RUN_COMMAND_RACECHECK = RUN_MODE="test" go run --race -ldflags $(LDFLAGS) cmd/compliance-service/main.go run --port $(PORT) --market-path "$(MARKET_PATH)" --profiles-path "$(PROFILES_PATH)" --es-url "$(ELASTICSEARCH_URL)" --postgres-uri "$(POSTGRES_URI)" --secrets-port $(SS_PORT)  --manager-port $(MANAGER_PORT) --event-endpoint $(EVENT_ENDPOINT) --inspec-tmp-dir "$(TMP_PATH)" $(DEV_CERT_PATHS) -- log-level "debug" --config /tmp/.compliance-service.toml
RUN_COMMAND_INFO = $(RUN_COMMAND) --log-level "info"
RUN_COMMAND_DEBUG = $(RUN_COMMAND) --log-level "debug"
TEST_PATH = "$(PWD)/api/tests"
RUN_GRPCURL_COMPLIANCE_VERSION = grpcurl --insecure -cert $(SSL_CERT) -key $(SSL_KEY) localhost:$(PORT) chef.automate.domain.compliance.api.version.VersionService/Version

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

clean-deps:
	rm -rf ../../vendor

## BUILD ##
build: build-linux build-windows build-mac

build-linux:
	# to ensure this build works in alpine
	# https://stackoverflow.com/questions/36279253/go-compiled-binary-wont-run-in-an-alpine-docker-container-on-ubuntu-host
	CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -tags netgo -ldflags $(LDFLAGS) -o dist/linux/compliance cmd/compliance-service/main.go

build-windows:
	GOOS=windows GOARCH=amd64 go build -ldflags $(LDFLAGS) -o dist/win/compliance.exe cmd/compliance-service/main.go

build-mac:
	GOOS=darwin GOARCH=amd64 go build -ldflags $(LDFLAGS) -o dist/mac/compliance cmd/compliance-service/main.go

## TOOLS ##
ruby-grpc-tools:
	@if ! gem list grpc -i; then gem install grpc -v 1.18.0 ; fi > /dev/null
	@if ! gem list grpc-tools -i; then gem install grpc-tools -v 1.18.0; fi > /dev/null

PROTOS = $(shell find . -name '*.proto' | sed 's/\.\///')
RUBY_PROTOS = $(patsubst %.proto, %_pb.rb, $(PROTOS))

EXTRA_PROTOS = '../../components/automate-grpc/protoc-gen-policy/iam/policy_pb.rb' '../../components/automate-grpc/protoc-gen-policy/api/policy_pb.rb' '../../components/automate-grpc/protoc-gen-policy/api/annotations_pb.rb' '../../components/automate-grpc/protoc-gen-policy/iam/annotations_pb.rb'

generate-ruby-grpc:
	@printf "\n===> ${blue}Generating ruby grpc files${reset}\n"
	$(MAKE) ruby-grpc-tools $(EXTRA_PROTOS) $(RUBY_PROTOS)
	$(MAKE) generate-ruby-grpc-nodemanager
	$(MAKE) generate-ruby-grpc-secrets-service

generate-ruby-grpc-nodemanager:
	cd ../nodemanager-service && make generate-ruby-grpc

generate-ruby-grpc-secrets-service:
	cd ../secrets-service && make generate-ruby-grpc

%_pb.rb : %.proto
	@printf " * ${blue}Compiling test proto${reset} for $<\n"
	@-grpc_tools_ruby_protoc -I. \
		-I../../ \
		-Iapi/ \
		-I$(GOPATH)/src \
		-I../../vendor \
		-I../../vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
		--ruby_out=. \
		--grpc_out=. \
		$<

check-go-version:
	@required_version=$(shell cat ../../GOLANG_VERSION); \
	my_version=$(shell go version | sed 's/.* go\([^ ]*\).*/\1/'); \
	if ! printf "$$required_version\n$$my_version\n" | sort -CV ; then \
		printf "===> ${red}Go version "$$my_version" found; $$required_version required.${reset}\n" ; \
		exit 1; \
	else \
		printf "===> ${green}Your go version "$$my_version" is good, moving on...${reset}\n" ; \
	fi;

proto:
	@printf "\n===> ${blue}Generating compliance-service protos${reset}\n"
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component compliance-service'

gateway-proto:
	@printf "\n===> ${blue}Generating automate-gateway protos${reset}\n"
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component automate-gateway'


wait-for-compliance-service:
	@i=1; \
	max_loops=30; \
	printf "\n===> ${blue}Checking Compliance Service version to ensure it's ready to receive connections...${reset}\n"; \
	$(RUN_GRPCURL_COMPLIANCE_VERSION); \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i Compliance Service is not running, retrying...${reset}\n"; \
	  sleep 4; \
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
	$(RUN_COMMAND_INFO) &> $(COMPLIANCE_LOG_PATH)&
	# since we background the service, we need to give it some time to start
	@$(MAKE) wait-for-compliance-service

run: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	@$(MAKE) check-go-version
	$(MAKE) run-nodemanager
	@printf "\n===> ${blue}Running compliance-service in background${reset} with logs redirected to ${yellow}$(COMPLIANCE_LOG_PATH)${reset}\n"
	$(RUN_COMMAND_DEBUG) &> $(COMPLIANCE_LOG_PATH)&
	# since we background the service, we need to give it some time to start
	@$(MAKE) wait-for-compliance-service

run-debug: clean build-inspec-runner $(MARKET_PATH) $(PROFILES_PATH)
	@$(MAKE) check-go-version
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
	@-pkill -f " run --port $(PORT)" || true
	@cd ../nodemanager-service && make clean


## ELASTICSEARCH, POSTGRES, SSH NODE ##
start-es-pg: start-pg create-es-data-dir
	@data_dir="$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)"; \
	if [ "$(A1_DATA)" == "true" ]; then \
	  printf "\n===> ${yellow}Loading test a1 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf generator/dump/es-pg/es5-a1-data.tar.gz -C "$$data_dir"; \
	  printf "===> ${yellow}Loading test a1 data into Postgres${reset}...\n"; \
	  docker cp generator/dump/es-pg/a1-pg-dump.sql.gz cc_pg:/tmp/; \
	  docker exec cc_pg rm -f /tmp/a1-pg-dump.sql; \
	  docker exec cc_pg gunzip /tmp/a1-pg-dump.sql.gz; \
	  docker exec cc_pg psql -Upostgres chef_automate -f /tmp/a1-pg-dump.sql; \
	fi; \
	if [ "$(A2V1_DATA)" == "true" ]; then \
		printf "\n===> ${yellow}Loading test A2v1 data into ElasticSearch${reset} ($$data_dir)...\n"; \
		tar -xzvf generator/dump/es-pg/es6-a2v1-data.tar.gz -C "$$data_dir"; \
	fi; \
	if [ "$(A2V2_DATA)" == "true" ]; then \
	  printf "\n===> ${yellow}Loading test A2v2 data into ElasticSearch${reset} ($$data_dir)...\n"; \
	  tar -xzvf generator/dump/es-pg/es6-a2v2-data.tar.gz -C "$$data_dir"; \
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
	docker rm -f cc_es5 &> /dev/null; docker run -d -v "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)":/usr/share/elasticsearch/data -v "$(PWD)/elasticsearch/config/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" --name cc_es5 -p $(ELASTICSEARCH_PORT):9200 -p 9305:9300 elasticsearch:5.4.0
	@$(MAKE) wait-for-es

start-es6: create-es-data-dir
	@printf "\n===> ${yellow}Recreating the ElasticSearch 6 container...${reset}\n"
	docker rm -f cc_es6 &> /dev/null; docker run -d -v "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)":/usr/share/elasticsearch/data -v "$(PWD)/elasticsearch/config/elasticsearch.yml:/usr/share/elasticsearch/config/elasticsearch.yml" --name cc_es6 -p $(ELASTICSEARCH_PORT):9200 -p 9305:9300 -e "xpack.security.enabled=false" docker.elastic.co/elasticsearch/elasticsearch:6.2.2
	@$(MAKE) wait-for-es

create-es-data-dir:
	@mkdir -p "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)" && chmod 777 "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)"

purge-es-data:
	rm -rf "$(PWD)/elasticsearch/.tmp"

# for automate 2.0, we'll be on pg6
start-pg:
	@printf "\n===> ${yellow}Recreating the PostgreSQL container...${reset}\n"
	docker rm -f cc_pg &> /dev/null; docker run --name cc_pg -e POSTGRES_DB=chef_automate -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=admin -p $(POSTGRES_PORT):5432 -d postgres:9.6.11 || docker start cc_pg
	@$(MAKE) wait-for-pg
	@printf "\n * ${blue}Creating the PostgreSQL databases...${reset}\n"
	docker exec -it cc_pg psql -Upostgres -c 'CREATE DATABASE secrets_service;'
	docker exec -it cc_pg psql -Upostgres -c 'CREATE DATABASE nodemanager_service;'

# Login to postgres and type away: \d, select * from nodes;, etc
login-pg:
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
	@printf "\n===> ${yellow}Clearing containers and directories ${reset}\n"
	docker rm -f cc_pg cc_es5 cc_es6 cc_ssh_node || true &> /dev/null
	@$(MAKE) purge-es-data
	rm -rf $(PROFILES_PATH)
	rm -rf $(MARKET_PATH)

start-ssh-node:
	@printf "\n===> ${blue}Ensuring we have a cc_ssh_node container${reset} to be used as ssh node target\n"
	docker rm -f cc_ssh_node &> /dev/null || true
	docker pull $(SSH_TARGET_IMAGE)
	docker run -p 11030:22 --name cc_ssh_node -d $(SSH_TARGET_IMAGE)

# need to use this here: pg_isready
# and change database to chef_compliance_service
wait-for-pg:
	@i=1; \
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
	@i=1; \
	max_loops=100; \
	printf "\n===> ${blue}Checking ElasticSearch to ensure it's ready to receive connections${reset}\n"; \
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
	@i=1; \
	max_loops=50; \
	printf "\n===> ${blue}Checking ElasticSearch to ensure it has ingested our reports${reset}\n"; \
	curl -s -X GET "$(ELASTICSEARCH_URL)/comp-*-r-20*/_doc/_count" | grep '"total":20,"successful":20' > /dev/null; \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i ElasticSearch is still ingesting, retrying...${reset}\n"; \
	  sleep 5; \
	  i=$$(( $$i + 1 )); \
	  curl -s -X GET "$(ELASTICSEARCH_URL)/comp-*-r-20*/_doc/_count" | grep '"total":20,"successful":20' > /dev/null; \
	done; \
	if [ $$i -eq $$max_loops ]; then \
	  printf "${red}Timeout waiting for ingestion to complete${reset}, cc_es$(ES_VER) logs:\n"; \
	  docker logs cc_es$(ES_VER) --since 2000; \
	  exit 1; \
	fi; \
	ls -la "$(PWD)/elasticsearch/.tmp/esdata$(ES_VER)";
	printf " * ${green}ElasticSearch finished successfully ingesting our reports!${reset}\n";

## Use this if you don't want to worry about the order in which to start things when testing.. it does it all!
run-test: clean-all test-prep run ingest-reports-into-es test

## TESTING ##
test-db: run-with-es-pg run-db-tests

run-db-tests:
	@POSTGRES_URI="$(POSTGRES_URI)" CGO_ENABLED=0 \
		go test -v $(shell go list ./dao/... ./profiles/db ./scanner/ ./inspec-agent/scheduler) -cover -database -parallel=1 -p 1 -failfast

test-integration: start-es start-pg
	printf "\n===> ${blue}Running integration tests with ES_VER=$(ES_VER)${reset}\n"
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
	printf "\n===> Running Automate upgrade integration tests with TEST=$(TEST)\n"
	@if [ "$(A1_DATA)" != "true" ] && [ "$(A2V1_DATA)" != "true" ] && [ "$(A2V2_DATA)" != "true" ]; then \
	  printf "===> ${red}Aborting test-automate-upgrade as one of these ENV variables is not true: A1_DATA, A2V1_DATA, A2V2_DATA${reset}\n"; \
	  exit 2; \
	fi;
	if ! [[ $(TEST) == 6* ]]; then \
	  printf "===> ${red}Aborting test-automate-upgrade as TEST ENV variable must start with 6, for example: 61_A2V1_migration_spec.rb${reset}\n"; \
	  exit 3; \
	fi;
	@$(MAKE) start-es-pg;
	@$(MAKE) run-log-level-info;
	# Give a few seconds for the background migrations to finish
	sleep 20;
	@if $(MAKE) test ; then \
	  printf "===> ${green}Automate upgrade integration tests passed!!!${reset}\n"; \
	else \
	  printf "===> ${red}Automate upgrade integration tests failed!!!${reset}\n"; \
	  exit 1; \
	fi;

test-integration-feed: start-es start-pg
	@printf "\n===> ${blue}Running integration tests with PG6 and ES_VER=$(ES_VER)${reset}\n"
	# we run the service locally so we can inspec detect and exec containers and ssh into containers
	export ELASTICSEARCH_URL
	sleep 10
	@if $(MAKE) test-feed ; then \
	  printf "===> ${green}feed integration tests passed!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}feed integration tests failed!!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-ingest-to-manager-conn:
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
	TEST='[2-5]*_spec.rb' $(MAKE) test

test-feed:
	@go test -v $(shell go list ./feed/integration_test/) -failfast

# Run individual tests with: TEST="02_nodes_spec.rb" make test
# or multiple ones with: TEST="02_*" make test
test: generate-ruby-grpc wait-for-compliance-service
	@if [ -z "$$TEST" ] ; then TEST='*_spec.rb'; fi; \
	cd api/tests && \
	printf "\n===> ${blue}Running 'bundle install' in api/tests...${reset}\n" && \
	bundle install && \
	printf "\n===> ${blue}Running the tests via run.rb...${reset}\n" && \
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
	bundle exec ./run.rb $$TEST \
	echo

test-prep: $(MARKET_PATH) $(PROFILES_PATH) start-es-pg start-ssh-node download-sample-market-profiles install-inspec

install-inspec:
	@inspec --version | grep -q $(shell cat ../../INSPEC_VERSION) || gem install inspec --no-document --version $(shell cat ../../INSPEC_VERSION)

test-examples:
	@printf "===> REQUIRES RUNNING SERVICE\n"
	$(MAKE) send-ingest-report
	cd examples/reporting/test && go test . -failfast
	cd examples/profiles/test && go test . -failfast

test-polling: start-ssh-node
	$(MAKE) test TEST="poll_test*"

test-unit:
	@printf "\n ===> ${blue}Running compliance-service go unit-tests...${reset}\n"
	@go test $(shell go list ./... |  grep -v '^github.com/chef/automate/components/compliance-service/\(examples\|feed/\|api/automate-feed\|api/automate-event\|api/tests\|scanner\|integration_test\)') -cover

send-ingest-report:
	go run examples/ingest/ingest_client.go --file ingest/examples/compliance-success-tiny-report.json --threads 1 --reports-per-thread 1

## DATA ##
$(MARKET_PATH):
	@mkdir -p $(MARKET_PATH)

$(PROFILES_PATH):
	@mkdir -p $(PROFILES_PATH)

download-sample-market-profiles: $(MARKET_PATH)
	@printf "\n===> ${blue}Downloading profiles...${reset}\n"
	@if [ -f $(MARKET_PATH)/linux-patch-baseline-0.3.0.tar.gz ]; then printf " * ${blue}linux-patch-baseline${reset} already downloaded\n"; else curl -LSs -o $(MARKET_PATH)/linux-patch-baseline-0.3.0.tar.gz https://github.com/dev-sec/linux-patch-baseline/archive/0.3.0.tar.gz; fi;
	@if [ -f $(MARKET_PATH)/windows-baseline-1.1.0.tar.gz ]; then printf " * ${blue}windows-baseline${reset} already downloaded\n"; else curl -LSs -o $(MARKET_PATH)/windows-baseline-1.1.0.tar.gz https://github.com/dev-sec/windows-baseline/archive/1.1.0.tar.gz; fi;

	@printf " * ${blue}Generating inspec jsons for $(MARKET_PATH)${reset}\n"
	@FILES=$$(ls -1 $(MARKET_PATH) | grep '\.tar.gz$$'); \
	for file in $$FILES; do \
	  file="$(MARKET_PATH)/$$file"; \
	  if [ -f $$file ]; then \
	    printf " * ${blue}$$file${reset} already generated\n"; \
	  else \
	    printf "\n >> Create inspec json for: ${blue}$$file${reset}\n"; \
	    inspec json "$$file" > "$$file.json"; \
	  fi; \
	done;

	@# we use this to test zip upload
	@if [ -f $(TMP_PATH)/apache-baseline-2.0.1.zip ]; then printf " * ${blue}apache-baseline-2.0.1${reset} already downloaded\n"; else curl -LSs -o $(TMP_PATH)/apache-baseline-2.0.1.zip https://github.com/dev-sec/apache-baseline/archive/2.0.1.zip; fi;
	@if [ -f $(TMP_PATH)/apache-baseline-2.0.2.zip ]; then printf " * ${blue}apache-baseline-2.0.2${reset} already downloaded\n"; else curl -LSs -o $(TMP_PATH)/apache-baseline-2.0.2.zip https://github.com/dev-sec/apache-baseline/archive/2.0.2.zip; fi;
	cp -f $(TEST_PATH)/mario-0.1.0.tar.gz $(TMP_PATH)

# when running against local es this requires start-es
# when running against es that is inside vm (virtualbox)  be sure to edit /opt/delivery/sv/elasticsearch/run and change -Des.network.host=127.0.0.1 to -Des.network.host=0.0.0.0
# in order to tunnel to a remote ES, we must trust the certificate that the remote instance is offering.  get this from your browser and add it to the keychain on the respective OS.
ingest-reports-into-es:
	@if  [ "$(A1_DATA)" != "true" ] && [ "$(A2V1_DATA)" != "true" ] && [ "$(A2V2_DATA)" != "true" ]; then \
	  @printf "\n===> PROCESSING REPORTS FROM ${blue}generator/dump/audit_reports${reset}\n"; \
	  FILES=$$(ls -1 generator/dump/audit_reports | grep '\.json'); \
	  for file in $$FILES; do \
	    file="generator/dump/audit_reports/$$file"; \
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
