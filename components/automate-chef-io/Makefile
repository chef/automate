# we use pushd/popd here, and /bin/sh of our chefes/buildkite image is not bash
# so we have to override the default shell here
SHELL=bash

themes/chef:
	git clone https://${GITHUB_TOKEN}@github.com/chef/chef-hugo-theme.git themes/chef

clean:
	rm -rf site/themes/chef

sync: themes/chef
	pushd themes/chef && git fetch && git reset --hard origin/master && popd

serve: sync
	hugo server --buildDrafts --noHTTPCache

lint: themes/chef
	hugo -D
