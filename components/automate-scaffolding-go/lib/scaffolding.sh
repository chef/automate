#!/bin/bash

source "$(pkg_path_for "$pkg_scaffolding")/lib/shared.sh"


scaffolding_load() {
    local lib
    lib="$(pkg_path_for "chef/scaffolding-go")/lib/scaffolding.sh"
    build_line "Loading Base Scaffolding $lib"
    if ! source "$lib"; then
        exit_with "Failed to load Base Scaffolding from $lib" 17
    fi

    pkg_scaffolding_old="$pkg_scaffolding"
    pkg_scaffolding="chef/scaffolding-go"
    if [[ "$(type -t scaffolding_load)" == "function" ]]; then
        scaffolding_load
    fi
    pkg_scaffolding="$pkg_scaffolding_old"
}
