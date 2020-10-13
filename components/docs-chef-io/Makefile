# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash
SWAGGER_RESULT_FILE=static/automate-api-docs/all-apis.swagger.json
SWAGGER_DIR=data/automate/api_chef_automate
SWAGGER_FILES=$(shell find $(SWAGGER_DIR) -name '*.swagger.json')
STATIC_SWAGGER_FILES=$(shell find data/automate/api-static/ -type f | sort -n)

assets:
	git submodule update --init --recursive
	git submodule foreach git pull origin master
	git submodule foreach git reset --hard
	pushd chef-web-docs/themes/docs-new && make assets && popd

clean:
	pushd chef-web-docs/themes/docs-new && make clean && popd
	rm -f $(SWAGGER_RESULT_FILE)
	rm -rf public/

clean_all: clean_swagger_files
	pushd chef-web-docs/themes/docs-new && make clean_all && popd
	rm -rf chef-web-docs/resources/
	rm -rf chef-web-docs/public/

clean_swagger_files:
	rm -rf ./$(SWAGGER_DIR)/*

preview_netlify: chef_web_docs_submodule
	pushd chef-web-docs && make assets; hugo --buildFuture --gc --minify --enableGitInfo && popd

chef_web_docs_submodule:
	git submodule update --init --recursive
	git submodule foreach git pull origin master
	git submodule foreach git reset --hard
	cp -R content/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/content
	cp -R static/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/static
	cp -R layouts/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/layouts

reset_chef_web_docs:
	git submodule foreach git reset --hard
	git submodule foreach git clean -f -d

update_chef_web_docs:
	git submodule update --remote --merge chef-web-docs

sync_swagger_files: clean_swagger_files
	pushd ../../api/external && \
		find . -name '*.swagger.json' | while read i ; do \
			mkdir -p "../../components/docs-chef-io/$(SWAGGER_DIR)/`dirname $$i`" && \
			cp $$i "../../components/docs-chef-io/$(SWAGGER_DIR)/$$i" \
		; done && \
	popd

	pushd ../automate-gateway/api/ && \
		find . -name '*.swagger.json' | while read i ; do \
			mkdir -p "../../docs-chef-io/$(SWAGGER_DIR)/`dirname $$i`" && \
			cp $$i "../../docs-chef-io/$(SWAGGER_DIR)/$$i" \
		; done && \
	popd

$(SWAGGER_RESULT_FILE): $(SWAGGER_FILES)

# This jq script is more complicated than it needs to be to workaround an issue where jq treats empty input as valid input:
# https://github.com/stedolan/jq/issues/1142
generate_swagger: $(SWAGGER_RESULT_FILE)
	swagger mixin -c=0 $(STATIC_SWAGGER_FILES) $(SWAGGER_FILES) | \
		jq -es 'if . == [] then null else .[] | .paths = (.paths | with_entries( select( all( .value[].tags[]; . != "hidden") ) ) ) end' \
		> $(SWAGGER_RESULT_FILE)

serve: generate_swagger assets
	hugo server --buildDrafts --noHTTPCache --buildFuture

lint: assets
	hugo -D

# Consult scripts/spellcheck.sh in the root Automate directory for notes on using cspell.
# This does files in docs-chef-io plus exposed proto files.
spellcheck:
	@pushd ../.. > /dev/null; \
		cspell \
			"api/external/**/*.proto" \
			"components/docs-chef-io/**/*.md" \
			"components/docs-chef-io/data/automate/api-static/**/*" \
			"components/docs-chef-io/layouts/**/*.html" \
			"components/automate-gateway/**/*.proto" \
		; EXIT_CODE=$$?; \
		popd > /dev/null; \
		exit $$EXIT_CODE
