#!/bin/bash

#shellcheck disable=SC2034
test_name="proxy"
test_proxy="true"

do_test_deploy() {
    source "${source_dir}/helpers/bldr_tests.sh"

    do_test_deploy_default
    bldr_smoke_test
}
