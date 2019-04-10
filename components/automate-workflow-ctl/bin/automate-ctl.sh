#!/usr/bin/env bash

# Pull in configuration from workflow-server
source /hab/svc/workflow-server/config/automate-ctl-config.sh
# CTL_ORIGIN here is replaced by sed in plan.sh on build
ctl_path=$(hab pkg path {{CTL_ORIGIN}}/automate-workflow-ctl)
export OMNIBUS_FILES="${ctl_path}/lib"
export RUBYLIB="${ctl_path}/lib"
export KNIFE_PATH="${ctl_path}/bin/knife"
bundler=$(hab pkg path "core/bundler")

cd $ctl_path
$bundler/bin/bundle exec bin/automate-ctl.rb "$@"
