#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

desired_inspec_version() {
    local top_level
    top_level=$(git rev-parse --show-toplevel)
    cat "$top_level/INSPEC_VERSION"
}

install_inspec() {
    hab pkg install chef/inspec/$(desired_inspec_version) -b -f
}

install_inspec
