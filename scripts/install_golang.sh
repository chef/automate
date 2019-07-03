#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

desired_golang_version() {
    local top_level
    top_level=$(git rev-parse --show-toplevel)
    cat "$top_level/GOLANG_VERSION"
}

install_hab_go() {
    local ident
    ident="core/go/$(desired_golang_version)"
    hab pkg install "$ident"
    hab pkg binlink "$ident" go --force
    hab pkg binlink "$ident" gofmt --force
}

install_hab_go
