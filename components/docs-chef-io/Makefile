# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash
SWAGGER_RESULT_FILE=static/automate-api-docs/all-apis.swagger.json
SWAGGER_DIR=data/automate/api_chef_automate
SWAGGER_FILES=$(shell find $(SWAGGER_DIR) -name '*.swagger.json')
STATIC_SWAGGER_FILES=$(shell find data/automate/api-static/ -type f | sort -n)

preview_netlify: chef_web_docs
	cp -R content/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/content
	cp -R data/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/data
	cp -R layouts/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/layouts
	cp -R static/* chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/static
	cp -R config.toml chef-web-docs/_vendor/github.com/chef/automate/components/docs-chef-io/
	pushd chef-web-docs && make bundle; hugo --buildFuture --gc --minify && popd

serve: generate_swagger chef_web_docs
	echo "replace github.com/chef/automate/components/docs-chef-io => ../" >> chef-web-docs/go.mod
	pushd chef-web-docs && make bundle; hugo server --buildDrafts --buildFuture --noHTTPCache --ignoreVendorPaths "github.com/chef/automate/**" && popd

chef_web_docs:
	if [ -d "chef-web-docs/" ]; then \
		pushd chef-web-docs && git reset HEAD --hard; git clean -fd; git pull --ff-only origin main; rm -rf public && popd; \
	else \
		git clone https://github.com/chef/chef-web-docs.git; \
	fi

clean_all: clean_swagger_files
	pushd chef-web-docs && make clean_all && popd

clean_swagger_files:
	rm -rf ./$(SWAGGER_DIR)/*

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
