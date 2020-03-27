ifneq ("$(wildcard ../../Makefile.common_go)", "")
include ../../Makefile.common_go
endif

PACKAGE_PATH = github.com/chef/automate/components/automate-deployment
CHEF_AUTOMATE_PKG = github.com/chef/automate/components/automate-cli/cmd/chef-automate
AUTOMATE_CTL_PKG = ${PACKAGE_PATH}/cmd/automate-ctl
CS_CTL_PKG = ${PACKAGE_PATH}/cmd/chef-server-ctl
PGDUMP_PKG = ${PACKAGE_PATH}/cmd/pg_dump
BINS = ${CHEF_AUTOMATE_PKG} ${PACKAGE_PATH}/cmd/deployment-service
GIT_SHA = $(shell git rev-parse HEAD)
BUILD_TIME ?= $(shell date -u '+%Y%m%d%H%M%S')
VERSION = ${BUILD_TIME}
GO_LDFLAGS = --ldflags "-X ${LIBRARY_PATH}/version.Version=${BUILD_TIME} -X ${LIBRARY_PATH}/version.GitSHA=${GIT_SHA} -X ${LIBRARY_PATH}/version.BuildTime=${BUILD_TIME}"
LINTERARGS=--exclude-use-default -D lll ./...
LICENSE_TOKEN = $(shell cat ${REPOROOT}/dev/license.jwt)
RESULT_DIR = ${REPOROOT}/results

build: ${BINS}

proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component automate-deployment'

package: habitat
	mkdir -p results
	cd ../.. && hab studio run "source .studiorc; build components/automate-deployment"

run: guard-HAB_ORIGIN
	@echo "Starting deployment service with --hartifacts=${RESULT_DIR} and --override-origin=${HAB_ORIGIN}..."
	./bin/linux/chef-automate deploy \
      --hartifacts ${RESULT_DIR} \
      --override-origin ${HAB_ORIGIN} \
      --channel dev \
      --upgrade-strategy none \
      --admin-password chefautomate \
      --accept-terms-and-mlsa
	./bin/linux/chef-automate license apply ${LICENSE_TOKEN}

run-with-stress: guard-HAB_ORIGIN
	@echo "Starting deployment service with --hartifacts=${RESULT_DIR} and --override-origin=${HAB_ORIGIN}..."
	./bin/linux/chef-automate deploy \
      --enable-deploy-order-stress-mode \
      --hartifacts ./results \
      --hartifacts ${RESULT_DIR} \
      --channel dev \
      --upgrade-strategy none \
      --admin-password chefautomate \
      --accept-terms-and-mlsa
	./bin/linux/chef-automate license apply ${LICENSE_TOKEN}

run-with-cs: guard-HAB_ORIGIN
	@echo "Starting deployment service with --enable-chef-server --hartifacts=${RESULT_DIR} and --override-origin=${HAB_ORIGIN}..."
	./bin/linux/chef-automate deploy \
      --enable-chef-server \
      --hartifacts ${RESULT_DIR} \
      --override-origin ${HAB_ORIGIN} \
      --channel dev \
      --upgrade-strategy none \
      --admin-password chefautomate \
      --accept-terms-and-mlsa
	./bin/linux/chef-automate license apply ${LICENSE_TOKEN}

run-with-workflow: guard-HAB_ORIGIN
	@echo "Starting deployment service with --enable-workflow --hartifacts=${RESULT_DIR} and --override-origin=${HAB_ORIGIN}..."
	./bin/linux/chef-automate deploy \
      --enable-workflow \
      --hartifacts ${RESULT_DIR} \
      --override-origin ${HAB_ORIGIN} \
      --channel dev \
      --upgrade-strategy none \
      --admin-password chefautomate \
      --accept-terms-and-mlsa
	./bin/linux/chef-automate license apply ${LICENSE_TOKEN}

run-upgrade-self-test: guard-HAB_ORIGIN
	@echo "Starting deployment service with --hartifacts=${RESULT_DIR} and --override-origin=${HAB_ORIGIN}..."
	./bin/linux/chef-automate migrate-from-v1 \
      --hartifacts ${RESULT_DIR} \
      --override-origin ${HAB_ORIGIN} \
      --channel dev \
      --upgrade-strategy none \
      --admin-password chefautomate \
			--self-test \
			--yes \
			--enable-chef-server
	./bin/linux/chef-automate license apply ${LICENSE_TOKEN}

ci: clean build unit lint fmt-check

${BINS}: bin echo-build-data
	@echo "GO $@"
	@cd bin; go build -i ${GO_LDFLAGS} $@

echo-build-data:
	@echo "build_time: ${BUILD_TIME}"
	@echo "   git_sha: ${GIT_SHA}"
	@echo "   version: ${VERSION}"

mocks:
	@echo "Regenerating mocks..."
	cd pkg/target && mockery -name=Target -case=underscore -inpkg -recursive
	cd pkg/events && mockery -name=EventSender -case underscore -inpkg -recursive
	cd ../../lib/platform/pg && mockery -name=DB -case underscore -inpkg -recursive

bin:
	mkdir -p bin

clean:
	rm -rf bin/*
        # Old data file
	rm -f ${RESULTS_DIR}/*deployment-service*.hart
	rm -f cover/*.coverage.out
	rm -f cover/cover.out

unit: golang_version_check
	@go test ./...

cover: golang_version_check
	@mkdir -p cover
	@go test -coverprofile=cover/cover.out ./...
	@echo "Coverage report written to cover/cover.out"
	@echo "To view:"
	@echo ""
	@echo "    go tool cover -html=cover/cover.out"

# https://stackoverflow.com/questions/4728810/makefile-variable-as-prerequisite
guard-%:
	@ if [ "${${*}}" = "" ]; then \
			echo "Environment variable $* not set"; \
			exit 1; \
	fi

HAVE_HABITAT := $(shell command -v hab 2> /dev/null)
habitat:
ifndef HAVE_HABITAT
	$(error "habitat not found on PATH. Please install habitat")
endif

# cross compilation targets
cross: linux darwin windows

# NB: the automate-ctl and pg_dump we create here are mock implementations used for testing.
CTL_EXE = automate-ctl
CS_CTL_EXE = chef-server-ctl
PGDUMP_EXE = pg_dump

linux darwin windows: OPTS = GOOS=$@ GOARCH=amd64

linux darwin windows:
	@make -f ../automate-cli/Makefile $@
	@echo Building ${CTL_EXE} for $@
	mkdir -p bin/$@
	cd bin/$@; \
	${OPTS} go build ${GO_LDFLAGS} ${AUTOMATE_CTL_PKG}
	@echo Done: bin/$@/${AUTOMATE_CTL_PKG}
	@echo Building ${PGDUMP_EXE} for $@
	cd bin/$@; \
	${OPTS} go build ${GO_LDFLAGS} ${PGDUMP_PKG}
	@echo Done: bin/$@/${PGDUMP_PKG}
	@echo Building ${CS_CTL_EXE} for $@
	mkdir -p bin/$@
	cd bin/$@; \
	${OPTS} go build ${GO_LDFLAGS} ${CS_CTL_PKG}
	@echo Done: bin/$@/${CS_CTL_PKG}

tools:
	mkdir -p bin/linux && cd bin/linux && GOOS=linux GOARCH=amd64 go build ../../tools/upgrade-test-scaffold/upgrade-test-scaffold.go

generate: update-bindings do_generate fmt

do_generate:
	go generate ./...

automate-ctl:
	mkdir -p bin/automate-ctl
	cd bin/automate-ctl; \
	go build -o automate-ctl -i ../../cmd/automate-ctl

update-bindings:
	bash ./scripts/agg_bindings.sh

check-bindings:
	@bash ./scripts/agg_bindings.sh -n

destroy:
	systemctl stop chef-automate
	pkill chef-automate || true
	rm -rf /hab/sup
	rm -rf /hab/svc
	rm -rf /hab/user
	userdel hab || true


HAB_AUTH_TOKEN?=$(shell go env HAB_AUTH_TOKEN)

hab-auth-token:
ifeq (${HAB_AUTH_TOKEN},)
	$(error "HAB_AUTH_TOKEN not set; do this: https://www.habitat.sh/docs/using-builder/#generate-an-access-token")
endif

# The a1-migration runs in a docker container. Unfortunately our
# preflight checks recommend sysctls that are not yet namespaced, so
# these commands must be run on the host:
a1-migration-host-setup:
	sysctl -w fs.file-max=64000
	sysctl -w vm.max_map_count=262144
	sysctl -w vm.dirty_ratio=15
	sysctl -w vm.dirty_expire_centisecs=20000
	sysctl -w vm.dirty_background_ratio=35


a1-migration-hab-keys:
	mkdir -p a1-migration/hab_keys
ifeq ("$(wildcard a1-migration/hab_keys/devchef*)","")
	cp ~/.hab/cache/keys/devchef* a1-migration/hab_keys/
endif

a1-migration-build: guard-HAB_ORIGIN
	docker build --build-arg AUTOMATE_CHANNEL=stable --build-arg AUTOMATE_VERSION=1.8.38 -m 4gb -t chef/a1-migration a1-migration

a1-migration-push:
	docker push chef/a1-migration

a1-migration-up: guard-HAB_ORIGIN
	cd a1-migration && docker-compose up -d && docker-compose exec a1-migration "a1-migration/run.sh"

a1-migration-down:
	cd a1-migration && docker-compose down

a1-migration-shell:
	cd a1-migration && docker-compose exec a1-migration /bin/bash

a1-migration-migrate: guard-HAB_ORIGIN
	cd a1-migration && docker-compose exec a1-migration bash -c 'HAB_ORIGIN=${HAB_ORIGIN} HARTIFACT_DIR=${HARTIFACT_DIR} /a1-migration/run_migrate_from_a1.sh'

AUTOMATE_API_DEFAULT_PASSWORD?=$(shell awk '/Admin password:/{print $$3}' a1-migration/keys/enterprise-test-admin-login-creds)
A1_BUILDER_PASSWORD?=$(shell awk '/Builder Password:/{print $$3}' a1-migration/keys/enterprise-test-admin-login-creds)
a1-migration-test:
	cd a1-migration && docker-compose exec a1-migration \
	  env HAB_LICENSE=accept-no-persist AUTOMATE_API_DEFAULT_PASSWORD=${AUTOMATE_API_DEFAULT_PASSWORD} \
	    A1_BUILDER_PASSWORD=${A1_BUILDER_PASSWORD} \
		inspec exec /a2/inspec/a2-migrate-from-v1-integration/

a1-migration-gather-logs:
	cd a1-migration && mkdir -p logs && docker-compose exec a1-migration bash -c "cd /a1-migration/logs && journalctl -u chef-automate > journal.log; chef-server-ctl gather-logs; automate-ctl gather-logs"

a1-migration-logs:
	cd a1-migration && docker-compose logs -f --tail=20

a1-migration-load-sample-data:
	cd a1-migration && docker-compose exec a1-migration /a1-migration/load-artifact

a1-migration-create-full-artifact: a1-migration-hab-keys
	cd a1-migration && docker-compose exec a1-migration bash -c 'HAB_ORIGIN=devchef /a1-migration/create_artifact.sh full'

a1-migration-create-minimal-artifact: a1-migration-hab-keys
	cd a1-migration && docker-compose exec a1-migration bash -c 'HAB_ORIGIN=devchef /a1-migration/create_artifact.sh minimal'

a1-migration-upload-artifact: hab-auth-token
	cd a1-migration && docker-compose exec a1-migration bash -c 'HAB_ORIGIN=devchef HAB_AUTH_TOKEN=${HAB_AUTH_TOKEN} hab pkg upload /a1-migration/results/*.hart'

basic-a1-build: guard-HAB_ORIGIN
	cp a1-migration/{delivery.license,internal_users.tsv,linux-patch-baseline-0.3.0.tar.gz} basic-a1/
	docker build --build-arg AUTOMATE_CHANNEL=stable --build-arg AUTOMATE_VERSION=1.8.38 -m 4gb -t devchef/basic-a1 basic-a1

basic-a1-start-chef-server:
	cd basic-a1 && docker-compose run --rm start_chef_server

basic-a1-up: guard-HAB_ORIGIN
	cd basic-a1 && docker-compose up -d && docker-compose exec basic-a1 "basic-a1/run.sh"

basic-a1-reconfigure-chef-server:
	cd basic-a1 && docker-compose exec chef-server chef-apply /volumes/keys/extra.rb

basic-a1-down: guard-HAB_ORIGIN
	cd basic-a1 && docker-compose down

basic-a1-migrate: guard-HAB_ORIGIN
	cd basic-a1 && docker-compose exec basic-a1 /basic-a1/chef-automate migrate-from-v1 --channel dev --upgrade-strategy none --yes --skip-backup-check --override-origin ${HAB_ORIGIN} --hartifacts /a2/results

basic-a1-diagnostics:
	cd basic-a1 && docker-compose exec basic-a1 /basic-a1/chef-automate diagnostics run

basic-a1-do-all: guard-HAB_ORIGIN basic-a1-build basic-a1-start-chef-server basic-a1-up basic-a1-reconfigure-chef-server basic-a1-migrate basic-a1-diagnostics basic-a1-test

basic-a1-gather-logs:
	cd basic-a1 && mkdir -p logs && docker-compose exec basic-a1 bash -c "cd /basic-a1/logs && journalctl -u chef-automate > journal.log; automate-ctl gather-logs"

basic_a1_automate_api_password?=$(shell awk '/Admin password:/{print $$3}' ./basic-a1/enterprise-test-admin-login-creds)
basic_a1_builder_password?=$(shell awk '/Builder Password:/{print $$3}' ./basic-a1/enterprise-test-admin-login-creds)
basic-a1-test:
	cd basic-a1 && docker-compose exec basic-a1 \
          env HAB_LICENSE=accept-no-persist AUTOMATE_API_DEFAULT_PASSWORD=${basic_a1_automate_api_password} \
            A1_BUILDER_PASSWORD=${basic_a1_builder_password} \
            NO_WORKFLOW_TEST=true \
                inspec exec /a2/inspec/a2-migrate-from-v1-integration/

.DEFAULT_GOAL := build
.PHONY: ${BINS} build proto clean cover tools
.PHONY: debugger mocks automate-ctl
.PHONY: cross darwin linux windows
.PHONY: echo-build-data destroy
.PHONY: hab-auth-token a1-migration-hab-keys
.PHONY: a1-migration-build a1-migration-push a1-migration-up a1-migration-down a1-migration-shell
.PHONY: a1-migration-logs a1-migration-create-artifact a1-migration-upload-artifact
.PHONY: basic-a1-chef-server-build basic-a1-build basic-a1-start-chef-server basic-a1-up basic-a1-down
.PHONY: basic-a1-migrate basic-a1-do-all basic-a1-diagnostics basic-a1-gather-logs basic-a1-reconfigure-chef-server
