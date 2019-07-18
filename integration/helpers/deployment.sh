#!/usr/bin/env bash

# $automate_config
# $hartifacts_path
# $manifest_path

install_hab() {
    local version
    local previous_umask

    version="$1"
    previous_umask=$(umask)

    # We need to mangle the umask here because it was set to "paranoid" above
    umask 022

    hab pkg install "core/hab/$version"
    hab pkg install "core/hab-sup/$version"
    hab pkg binlink "core/hab/$version" --force

    umask "$previous_umask"
}

serve_manifest() {
    local manifest_path="$1"
    upgrade-test-scaffold setup "$manifest_path"
    upgrade-test-scaffold serve "$manifest_path" &
    sleep 5
}

run_upgrade() {
    local airgap_artifact_path="$1"

    # NOTE: This is a bit of a hack.
    #
    # The deployment-service determines the upgrade status by comparing the package
    # versions in it's manifest with the package versions that are currently
    # installed and running. The manifest is cached and only upgrade periodically.
    # To trigger an upgrade we need the manifest to include our new hartifacts.
    # Moving the hartifacts into the directory won't trigger a manifest rebuild
    # because that directory isn't watched for changes. Therefore, we'll trigger
    # a manifest rebuild with the run command.
    if [ -z "$airgap_artifact_path" ]; then
        chef-automate upgrade run
    else
        chef-automate upgrade run --airgap-bundle "$airgap_artifact_path"
    fi

    # NOTE: This is a hack
    # The hack above was no longer good enough because we have a thing that needs
    # to be updated that isn't a service
    sleep 45

    #shellcheck disable=SC2154
    wait_for_upgrade "$test_detect_broken_cli" "$test_detect_broken_packages"
}

wait_for_upgrade() {
    local check_for_broken_cli
    local check_for_broken_packages
    check_for_broken_cli="$1"
    check_for_broken_packages="$2"

    log_info "Waiting for services to upgrade"
    upgrade_complete="false"
    for try in {1..90}; do
        log_info "Checking upgrade status (try $try/90)"
        local upgrade_status_output
        local errcode
        errcode="0"
        upgrade_status_output="$(chef-automate upgrade status -d 2>&1)" || errcode="$?"
        echo "${upgrade_status_output}"
        echo "status exit code=$errcode"

        case "$errcode" in
            0)
                :
                ;;
            98|99)
                log_warning "Error calling deployment service"
                ;;
            *)
                if [[ "$check_for_broken_cli" = true ]]; then
                    fix_broken_cli
                else
                    return 1
                fi
        esac

        if echo "${upgrade_status_output}" | grep 'up-to-date'; then
            upgrade_complete="true"
            break
        else
            if [[ "$check_for_broken_packages" = true ]]; then
                fix_broken_packages
            fi
            log_info "Retrying in 10 seconds"
            sleep 10
        fi
    done

    if [[ "$upgrade_complete" != "true" ]]; then
        echo "Services failed to upgrade in a reasonable amount of time."
        echo "Final upgrade status:"
        chef-automate upgrade status
        return 1
    fi

    # TODO(ssd) 2018-10-17: Ugh. If an upgrade /just/ upgrades
    # configuration, it might take a while for all of the services to
    # get restarted by habitat, making our standard wait_for_healthy
    # function incorrect if it runs too soon.
    log_info "Sleeping 10 seconds because mistakes were made"
    sleep 10


    wait_for_healthy
}

fix_broken_cli() {
    log_info "Checking for broken CLI"
    if ! chef-automate -h; then
        log_info "Fixing"
        rm ~/.chef-automate/executable-cache/*
    else
        log_info "The CLI appears to be working fine"
        log_info "Carry on"
    fi
}

fix_broken_packages() {
    log_info "Checking for broken Habitat packages"
    declare -a packages_to_check=("core/hab-sup")

    for pkg in "${packages_to_check[@]}"; do
        for pkg_dir in "/hab/pkgs/$pkg"/*/*; do
            # TODO(ssd) 2018-12-07: This is an incomplete check,
            # really any number of files can be missing. A complete
            # check would look at FILES and check every file in the
            # package.
            if [[ ! -f "$pkg_dir/TARGET" ]]; then
                log_error "BROKEN PACKAGE DETECTED (attempting fix): $pkg_dir"
                rm -rf "$pkg_dir"
                ident=$(echo "$pkg_dir" | cut -d/ -f4-7)
                hab pkg install "$ident"
            fi
        done
    done
}


wait_for_healthy() {
  chef-automate status -d -w -t 240 -r 5
}

build_bundle() {
    local output_path="$1"
    local manifest_path="$2"
    local hartifacts_path="$3"
    local args=(
        "--manifest" "${manifest_path}"
        "--override-origin" "$HAB_ORIGIN"
    )

    # We need to make sure the harts are installed so that the bundle creation works
    if [ -n "${hartifacts_path}" ]
    then
        args+=(
            "--hartifacts" "${hartifacts_path}"
        )

        if ls "${hartifacts_path}"/*.hart
        then
            log_info "Installing harts"
            hab pkg install "${hartifacts_path}"/*.hart
        fi
    fi

    log_info "Creating airgap bundle"
    chef-automate airgap bundle create "${args[@]}" "${output_path}"
}

download_cli() {
    local release="$1"
    local dst="$2"

    curl "https://packages.chef.io/files/current/automate/$release/chef-automate_linux_amd64.zip" | gunzip - > "$dst" && chmod +x "$dst"
}

download_manifest() {
    local channel="$1"
    local dst="$2"

    curl "https://packages.chef.io/manifests/$channel/automate/latest.json" > "$dst"
}
