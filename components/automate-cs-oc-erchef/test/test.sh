#!/bin/bash -e

export WEBUI_KEY=/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem
export SUPERUSER_KEY=/hab/svc/automate-cs-oc-erchef/data/pivotal.pem

# workaround for https://github.com/chef/ci-studio-common/pull/130
chmod 0600 /root/.netrc

CHEF_SERVER_CTL_PATH=$(hab pkg path chef/chef-server-ctl)

BUNDLE_GEMFILE="${CHEF_SERVER_CTL_PATH}/oc-chef-pedant/Gemfile"
export BUNDLE_GEMFILE

time hab pkg exec core/bundler \
  bundle exec "${CHEF_SERVER_CTL_PATH}/oc-chef-pedant/bin/oc-chef-pedant" \
  -c /hab/svc/automate-cs-oc-erchef/config/pedant_config.rb \
  --skip-knife --skip-oc_id --skip-license --skip-controls

