API_DOC_HTML=priv/static/docs/index.html
RAML2HTML=node_modules/.bin/raml2html
WATCH=node_modules/.bin/watch

all: compile lint dialyzer test
.PHONY: all

static: install lint
unit: install ci_test
.PHONY: static unit

install:
	mix local.hex --force
	mix local.rebar --force
	mix escript.install github tony612/protobuf-elixir --force

deps:
	mix deps.get
.PHONY: deps

ELIXIR_PROTOC=PATH=$(PATH):~/.mix/escripts protoc --elixir_out=plugins=grpc:./lib/pb -I priv priv/notifications.proto
BASE_PROTO_FILES=priv/notifications.proto priv/rules.proto priv/server.proto priv/health.proto

elixir_client:
	$(ELIXIR_PROTOC) $(BASE_PROTO_FILES) priv/google/protobuf/struct.proto

gen-proto: deps elixir_client

compile: gen-proto
	mix compile

.PHONY: compile

# Simple compile target with no dependency on deps.
# Use only after you've run a regular 'compile' or 'deps'
compilef: deps
	mix compile

test: compile
	SERVICE_CERT=../../../dev/certs/notifications-service.crt \
	SERVICE_KEY=../../../dev/certs/notifications-service.key \
	ROOT_CA_CERT=../../../dev/certs/Chef_Automate_FAKE_Dev.crt \
		mix test --no-start
.PHONY: test

ci_test: compilef
	mix test --no-start
.PHONY: ci_test

dialyzer: compilef
	mix dialyzer
.PHONY: dialyzer

serve: compile
	SERVICE_CERT=../../../dev/certs/notifications-service.crt \
	SERVICE_KEY=../../../dev/certs/notifications-service.key \
	ROOT_CA_CERT=../../../dev/certs/Chef_Automate_FAKE_Dev.crt \
		elixir --name notifications@127.0.0.1 -S mix do run --no-halt
.PHONY: serve

release: deps docs
	MIX_ENV=prod mix release
.PHONY: release

clean:
	rm -rf _build node_modules deps
	rm -f $(API_DOC_HTML)
.PHONY: clean

console:
	docker-compose exec notifications sh -c 'iex --remsh notifications@127.0.0.1 --name client@127.0.0.1'
.PHONY: console

lint: compilef
	mix credo

priv/static/docs:
	mkdir -p $@

PORT ?= 4001
run_release: release
	PORT=$(PORT) _build/prod/rel/notifications/bin/notifications foreground
.PHONY: run_release


