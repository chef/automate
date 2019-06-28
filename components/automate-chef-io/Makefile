# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash

themes/chef:
	git clone https://${GITHUB_TOKEN}@github.com/chef/chef-hugo-theme.git themes/chef

clean:
	rm -rf site/themes/chef

sync: themes/chef
	pushd themes/chef && git fetch && git reset --hard origin/master && popd

SWAGGER_DIR=data/docs/api_chef_automate

clean_swagger_files:
	rm -rf $(SWAGGER_DIR)/*

sync_swagger_files: clean_swagger_files
	pushd ../../api/external && find . -name '*.swagger.json' | cpio -p -dumv ../../components/automate-chef-io/$(SWAGGER_DIR) && popd
	pushd ../automate-gateway/api/ && find . -name '*.swagger.json' | cpio -p -dumv ../../automate-chef-io/$(SWAGGER_DIR) && popd

serve: sync
	hugo server --buildDrafts --noHTTPCache

lint: themes/chef
	hugo -D
