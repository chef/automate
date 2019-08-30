# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash
SWAGGER_RESULT_FILE=static/api-docs/all-apis.swagger.json
SWAGGER_DIR=data/docs/api_chef_automate
#SWAGGER_FILES=$(shell find $(SWAGGER_DIR) -name '*.swagger.json')
SWAGGER_FILES=data/docs/api_chef_automate/compliance/reporting/reporting.swagger.json data/docs/api_chef_automate/gateway/gateway.swagger.json
STATIC_SWAGGER_FILES=$(shell find data/docs/api-static/ -type f | sort -n)

EXPECTED_CONFLICTS=46

themes/chef:
	scripts/clone_hugo.sh

clean:
	rm -rf site/themes/chef
	rm -f $(SWAGGER_RESULT_FILE)
	rm -rf themes/
	rm -rf public/

sync: themes/chef
	pushd themes/chef && git fetch && git reset --hard origin/master && popd

clean_swagger_files:
	rm -rf ./$(SWAGGER_DIR)/*

sync_swagger_files: clean_swagger_files
	pushd ../../api/external && \
		find . -name '*.swagger.json' | while read i ; do \
			mkdir -p "../../components/automate-chef-io/$(SWAGGER_DIR)/`dirname $$i`" && \
			cp $$i "../../components/automate-chef-io/$(SWAGGER_DIR)/$$i" \
		; done && \
	popd

	pushd ../automate-gateway/api/ && \
		find . -name '*.swagger.json' | while read i ; do \
			mkdir -p "../../automate-chef-io/$(SWAGGER_DIR)/`dirname $$i`" && \
			cp $$i "../../automate-chef-io/$(SWAGGER_DIR)/$$i" \
		; done && \
	popd

$(SWAGGER_RESULT_FILE): $(SWAGGER_FILES)

#generate_swagger: $(SWAGGER_RESULT_FILE)
#	swagger mixin --quiet -c=$(EXPECTED_CONFLICTS) $(SWAGGER_FILES) > $(SWAGGER_RESULT_FILE)

generate_swagger: $(SWAGGER_RESULT_FILE)
	swagger mixin $(STATIC_SWAGGER_FILES) $(SWAGGER_FILES) > $(SWAGGER_RESULT_FILE)

serve: sync generate_swagger
	hugo server --buildDrafts --noHTTPCache

lint: themes/chef
	hugo -D

build: themes/chef generate_swagger
	hugo
