# This Makefile written by concrete
#
# {concrete_makefile_version, 1}
#
# Use this to override concrete's default dialyzer options of
# -Wunderspecs
# DIALYZER_OPTS = ...

# List dependencies that you do NOT want to be included in the
# dialyzer PLT for the project here.  Typically, you would list a
# dependency here if it isn't spec'd well and doesn't play nice with
# dialyzer or otherwise mucks things up.
#
DIALYZER_SKIP_DEPS = gun shotgun

# If you want to add dependencies to the default "all" target provided
# by concrete, add them here (along with make rules to build them if needed)
# ALL_HOOK = ...

SHELL := /bin/bash

CT_DIR = test/ct

PROJ = delivery

concrete_rules_file = $(wildcard concrete.mk)
ifeq ($(concrete_rules_file),concrete.mk)
    include concrete.mk
else
    all:
	@echo "ERROR: missing concrete.mk"
	@echo "  run: concrete update"
endif

# Deploy the latest schema changes without having to run `delivery-ctl reconfigure`
sqitch_deploy:
	su - delivery -c "sqitch --engine pg --db-name delivery --top-dir /opt/delivery/embedded/service/delivery_schema deploy --verify"

# resets the schema for a given DB belonging to a given user
reset-schema-for:
	su - $(DB_USER) -c "cd `pwd`/schema; sqitch --db-name $(DB_NAME) revert -y" \
		|| (su - $(DB_USER) -c 'psql --dbname template1 --single-transaction --command "DROP DATABASE IF EXISTS $(DB_NAME)"' \
			  && su - chef-pgsql -c "psql --dbname template1 --single-transaction --command 'CREATE DATABASE $(DB_NAME) OWNER $(DB_USER)'")
	su - $(DB_USER) -c "cd `pwd`/schema; sqitch --db-name $(DB_NAME) deploy"
	# I initially wanted to make the 'CREATE EXTENSION' part of the schema, but
	# you need to be pgsql's root to do that :-/
	su - chef-pgsql -c "psql --dbname $(DB_NAME) --command 'CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";'"

delivery-db-vars:
	$(eval DB_USER = delivery)
	$(eval DB_NAME = delivery)

# resets the schema for your delivery service (use with care!)
reset-delivery-schema: delivery-db-vars reset-schema-for

test_schema:
	su - chef-pgsql -c "make -C `pwd`/schema round_trip"

# RELX_OUTPUT_DIR is a make variable provided by concrete.mk and it's
# wrapping of relx. If unset, the default is _rel. Relx builds the
# release in this directory.
dev-sys-config:
	cd $(RELX_OUTPUT_DIR)/delivery/releases/0.0.1; ln -snf /var/opt/delivery/delivery/etc/sys.config
	cd $(RELX_OUTPUT_DIR)/delivery/releases/0.0.1; ln -snf /var/opt/delivery/delivery/etc/vm.args
	cd $(RELX_OUTPUT_DIR); ln -snf /var/log/delivery/delivery delivery/log

compile_fast:
	time $(REBAR) compile skip_deps=true

# Runs only test suites beginning with a_* (which are the suites running
# in acceptance against the actual acceptance API)
ct_acceptance: clean_ct
	time $(REBAR) ct skip_deps=true suites=$$(find apps/*/$(CT_DIR) -name 'a_*_SUITE.erl' -exec basename {} _SUITE.erl \; | tr '\n' ',')

## This is used for the dev-mode delivery server only
enterprise_ctl: apps/enterprise_ctl/src/enterprise_ctl.erl
	cd apps/enterprise_ctl; make distclean all_but_dialyzer_and_eunit escriptize
	mkdir -p $(RELX_OUTPUT_DIR)/delivery/bin/
	cp apps/enterprise_ctl/enterprise_ctl $(RELX_OUTPUT_DIR)/delivery/bin/

## Use this target on the vm to obtain an Erlang shell that will
## automatically recompile and reload modules (including eunit and CT
## modules) and run eunit and CT tests ala guard.
sync_tests: compile
	@rm -rf ebin-tests && mkdir ebin-tests
	@erlc -o ebin-tests ../scripts/sync_tests.erl
	@$(ERL) -pa deps/*/ebin -pa ebin-tests -s sync_tests start

xref:
	$(REBAR) skip_deps=true xref

link-rel: rel with_patches
	@RELX_RELEASE_DIR=$(RELX_RELEASE_DIR) ../scripts/link-rel.sh

with_patches:
	rsync -a --exclude=*.app apps/patches/ebin/ $(RELX_OUTPUT_DIR)/delivery/lib/patches/

## Local Dev Console
LOCAL_DEV_REL=/tmp/delivery_rel
rebuild_local:
	sudo rm -Rf $(LOCAL_DEV_REL)
	RELX_OUTPUT_DIR=$(LOCAL_DEV_REL) PROJ=delivery EUNIT_DROP_DATABASE=1 make allclean link-rel dev-sys-config

start_local_console:
	sudo $(LOCAL_DEV_REL)/delivery/bin/delivery console

syntax: syntax_elvis syntax_dialyzer

# For now we are separating the compile from the dialyzer
# execution. Because of the way we collect the Make variables, on a
# clean build, you can't get a working dialyzer run.
syntax_dialyzer:
	$(MAKE) compile
	$(MAKE) dialyzer

syntax_elvis:
	./elvis rock

# /signals/db_ready is written by the schema makefile. It is intended only for
# use in the test docker-compose environment. Waits a maximum of 10 minutes.
wait_for_signal:
	@echo "Waiting for sqitch to finish prepping database"
	i=0; while [ ! -f /signals/db_ready ]; do if [ "$$i" -eq "60" ]; then exit 1; else echo "$$(date) - $$i still waiting for db"; i=$$[i+1]; sleep 10; fi; done
