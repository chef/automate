# lll: ignore long lines
# unparam: unused parameters
# nakedret: naked returns in long functions
# prealloc: slices that could be preallocated
LINTERARGS=-v -D lll -D unparam -D nakedret -D prealloc --deadline 1m ./...
include ../../Makefile.common_go

# Replaces 'unknown' terminal with 'xterm' for 'tput' colors to work in buildkite
TERM := $(subst unknown,xterm,$(TERM))

# https://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux
red=`tput setaf 1`
green=`tput setaf 2`
yellow=`tput setaf 3`
blue=`tput setaf 6`
reset=`tput sgr0`

# certificates
SSL_CERT="$(PWD)/../../dev/certs/nodemanager-service.crt"
SSL_KEY="$(PWD)/../../dev/certs/nodemanager-service.key"
SSL_ROOT_CERT="$(PWD)/../../dev/certs/Chef_Automate_FAKE_Dev.crt"
EVENT_SSL_CERT="$(PWD)/../../dev/certs/event-service.crt"
EVENT_SSL_KEY="$(PWD)/../../dev/certs/event-service.key"
SS_SSL_CERT="$(PWD)/../../dev/certs/secrets-service.crt"
SS_SSL_KEY="$(PWD)/../../dev/certs/secrets-service.key"


# postgres
POSTGRES_HOST ?= 127.0.0.1
POSTGRES_PORT ?= 15432
POSTGRES_USER ?= postgres
POSTGRES_PASS ?= admin
POSTGRES_URI  ?= postgresql://$(POSTGRES_USER):$(POSTGRES_PASS)@$(POSTGRES_HOST):$(POSTGRES_PORT)/nodemanager_service?sslmode=disable
POSTGRES_URI_SS ?= postgresql://$(POSTGRES_USER):$(POSTGRES_PASS)@$(POSTGRES_HOST):$(POSTGRES_PORT)/secrets_service?sslmode=disable

PORT      ?= 10120

# secrets-service
SS_PORT   ?= 10131
SECRETS_KEY ?= 12345678901234567890123456789012
SS_CONFIG_PATH ?= /tmp/secrets-service.toml
SS_LOG_PATH    ?= /tmp/secrets-service.log
RUN_SS_COMMAND_DEBUG = go run ../secrets-service/cmd/secrets-service/secrets-service.go serve --config $(SS_CONFIG_PATH)
SS_MIGRATIONS_PATH="$(PWD)/../secrets-service/dao/migration/sql/"
define SS_CONFIG_CONTENT
[secretskey]
key = "$(SECRETS_KEY)"

[service]
host = "127.0.0.1"
port = $(SS_PORT)

[tls]
cert_path = $(SS_SSL_CERT)
key_path = $(SS_SSL_KEY)
root_ca_path = $(SSL_ROOT_CERT)

[postgres]
uri = "$(POSTGRES_URI_SS)"
migrations_path = $(SS_MIGRATIONS_PATH)
endef
export SS_CONFIG_CONTENT

create-ss-config:
	echo "$$SS_CONFIG_CONTENT" > $(SS_CONFIG_PATH)

MANAGER_LOG_PATH ?= /tmp/nodemanager-service.log
RUN_GRPCURL_NODEMANAGER_CHECK = grpcurl --insecure -cert $(SSL_CERT) -key $(SSL_KEY) localhost:$(PORT) grpc.health.v1.Health.Check

# events-service
EVENT_ENDPOINT    ?= 127.0.0.1:10132
EVENT_CONFIG_PATH ?= /tmp/event-service.toml
EVENT_LOG_PATH    ?= /tmp/event-service.log
RUN_EVENT_SERVICE_COMMAND = go run ../event-service/cmd/event-service/event-service.go serve --config $(EVENT_CONFIG_PATH)
define EVENT_CONFIG_CONTENT
[service]
host = "localhost"
port = 10132
event_limit = 100000
listener_limit = 10000

[tls]
cert_path = $(EVENT_SSL_CERT)
key_path = $(EVENT_SSL_KEY)
root_ca_path = $(SSL_ROOT_CERT)

[mlsa]
accept = false

[log]
format = "text"
level = "debug"

[handlers]
feed = "0.0.0.0:10121"
cfgingest = "0.0.0.0:10122"
endef
export EVENT_CONFIG_CONTENT

create-event-config:
	echo "$$EVENT_CONFIG_CONTENT" > $(EVENT_CONFIG_PATH)

##### RUN COMMANDS
run-with-1-min-polls:
	printf "TODO make this a thing\n"

# Runs the nodemanager and dependent services in background
run: clean create-ss-config create-event-config
	mkdir -p /tmp/
	@printf "\n===> ${blue}Running secret-service in background${reset} with logs redirected to ${yellow}$(SS_LOG_PATH)${reset}\n"
	$(RUN_SS_COMMAND_DEBUG) &> $(SS_LOG_PATH)&
	@printf "\n===> ${blue}Running event-service in background${reset} with logs redirected to ${yellow}$(EVENT_LOG_PATH)${reset}\n"
	$(RUN_EVENT_SERVICE_COMMAND) &> $(EVENT_LOG_PATH)&
	@printf "\n===> ${blue}Running nodemanager-service in background${reset} with logs redirected to ${yellow}$(MANAGER_LOG_PATH)${reset}\n"
	go run cmd/nodemanager-service/nodemanager.go run --port $(PORT) --cert $(SSL_CERT) --key $(SSL_KEY) --root-cert $(SSL_ROOT_CERT) --postgres-uri $(POSTGRES_URI) --secrets-port $(SS_PORT) --event-endpoint $(EVENT_ENDPOINT) &> $(MANAGER_LOG_PATH)&
	@$(MAKE) wait-for-nodemanager-service

run-compliance-service:
	cd ../compliance-service && make test-prep
	cd ../compliance-service && make run
	sleep 10

##### DATABASE
start-pg:
	cd ../compliance-service && make start-pg

login-pg:
	docker exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" -it cc_pg psql -Upostgres nodemanager_service

clear-pg:
	docker exec -it cc_pg psql -Upostgres nodemanager_service -c 'TRUNCATE nodes, node_managers, tags CASCADE;'


##### TESTING
test-unit:
	@go test -v $(shell go list ./... |  grep -v '/tests') -cover

ruby-grpc-tools:
	@if ! gem list grpc -i; then gem install grpc; fi > /dev/null
	@if ! gem list grpc-tools -i; then gem install grpc-tools; fi > /dev/null

PROTOS = $(shell find . -name '*.proto' | sed 's/\.\///')
RUBY_PROTOS = $(patsubst %.proto, %_pb.rb, $(PROTOS))

generate-ruby-grpc: ruby-grpc-tools $(RUBY_PROTOS)

%_pb.rb : %.proto
	@printf "\n * Compiling test proto for $<\n"
	@-grpc_tools_ruby_protoc -I. \
		-I../.. \
		-I$(GOPATH)/src \
		-I../../vendor \
		-I../../vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
		--ruby_out=. \
		--grpc_out=. \
		$<

test-integration-nodemanager: run run-compliance-service
	@printf "\n===> ${yellow}Running integration tests${reset} with PG6 and ES_VER=$(ES_VER)\n"
	@if $(MAKE) test-integration ; then \
	  printf "===> ${green}Tests passed, you are the boss!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}Tests failed, what happened?!?!?!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-integration-db: start-pg run
	@printf "\n===> ${yellow}Running db integration tests${reset} with PG6\n"
	@if $(MAKE) run-db-tests ; then \
	  printf "===> ${green}Tests passed, you are the boss!!!${reset}\n"; \
	  $(MAKE) clean; \
	else \
	  printf "===> ${red}Tests failed, what happened?!?!?!!${reset}\n"; \
	  $(MAKE) clean; \
	  exit 1; \
	fi;

test-integration:
	go clean -testcache
	RUN_MODE='local' go test github.com/chef/automate/components/nodemanager-service/tests
	RUN_MODE='local' go test github.com/chef/automate/components/nodemanager-service/tests/scanjobtests

run-db-tests:
	@POSTGRES_URI="$(POSTGRES_URI)" \
		go test -v $(shell go list ./pgdb/...) -cover -database

test-compliance-ingestion-to-manager-conn: run-background
	cd ../compliance-service && make run
	go run ../compliance-service/examples/ingest/ingest_client.go --file ../compliance-service/ingest/examples/compliance-success-tiny-report.json
	go run ../compliance-service/examples/ingest/ingest_client.go --file ../compliance-service/ingest/examples/compliance-failure-big-report.json
	# now send the first report in again, to ensure we get two, not three nodes
	go run ../compliance-service/examples/ingest/ingest_client.go --file ../compliance-service/ingest/examples/compliance-success-tiny-report.json
	$(MAKE) test TEST="test_ingest_to_mgr*"

proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component nodemanager'

clean:
	@printf "\n===> ${yellow}Clearing nodemanager and dependent services...${reset}\n"
	@# dirs(create-ss-config, create-event-config) have been created by a bug in the past and confuse Makefile targets with the same name
	@rm -rf create-ss-config create-event-config
	@-pkill -f "nodemanager.* run --port" || true
	@-pkill -f "event-service.* serve --config" || true
	@-pkill -f "secrets-service.* serve --config" || true

wait-for-nodemanager-service:
	@i=1; \
	max_loops=20; \
	printf "\n===> ${blue}Checking NodeManager Service health to ensure it's ready to receive connections...${reset}\n"; \
	$(RUN_GRPCURL_NODEMANAGER_CHECK); \
	while [ $$? -ne 0 -a $$i -lt $$max_loops ]; do \
	  printf " * ${yellow}$$i NodeManager Service is not running, retrying...${reset}\n"; \
	  sleep 3; \
	  i=$$(( $$i + 1 )); \
	  $(RUN_GRPCURL_NODEMANAGER_CHECK); \
	done; \
	if [ $$i -eq $$max_loops ]; then \
	  printf "${red}Giving up checking for NodeManager Service version${reset}\n"; \
	  exit 1; \
	fi; \
	printf " * ${green}NodeManager Service is running, moving on...${reset}\n"
