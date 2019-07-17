#!/bin/bash
#shellcheck disable=SC2034
test_name="workflow"

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --enable-workflow \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_test_deploy() {
    workflow-ctl create-enterprise automate --ssh-pub-key-file /hab/svc/automate-workflow-server/var/etc/builder_key.pub
    do_test_deploy_default
}
