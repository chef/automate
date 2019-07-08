#!/usr/bin/env bash

build_changed() {
    hab pkg install core/ruby
    export PATH=$PATH:$(hab pkg path core/ruby)/bin
    sudo $(hab pkg path core/ruby)/bin/gem install toml

    build_commands=""
    for component in $("$(a2_root_dir)/scripts/changed_components.rb")
    do
        build_commands="$build_commands build $component;"
    done

    if [ "$build_commands" != "" ]
    then
        hab studio $STUDIO_OPTS run "source .studiorc; set -e; $build_commands"
    fi
}

download_hartifacts() {
    "$(a2_root_dir)/scripts/download_verify_harts.sh"
}

build_tools() {
    local deployment_root_dir
    deployment_root_dir="$(a2_root_dir)/components/automate-deployment"

    pushd "$(a2_root_dir)/components/automate-deployment"
    make linux
    make tools
    popd

    export PATH="$deployment_root_dir/bin/linux/:$PATH"
}

create_manifest() {
    local dst="$1"
    ALLOW_LOCAL_PACKAGES=true "$(a2_root_dir)/.expeditor/create-manifest.rb"
    mv manifest.json "$dst"
}

copy_hartifacts() {
    "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$1"
}

a2_root_dir() {
    echo "$A2_ROOT_DIR"
}

sync_a1_migration_data() {
    log_info "Syncing a1 migration data"

    local previous_umask
    previous_umask=$(umask)

    umask 022

    hab sup run &

    set +x
    until hab svc status &> /dev/null; do
      echo "waiting for hab-sup to come up"
      sleep 1
    done
    set -x

    DATA_PACKAGE_RELEASE="20181206000427"
    # hab pkg install "$(a2_root_dir)/results/devchef-a1-migration-data-minimal-0.0.1-${DATA_PACKAGE_RELEASE}-x86_64-linux.hart"
    hab pkg install "devchef/a1-migration-data-minimal/0.0.1/$DATA_PACKAGE_RELEASE" --channel unstable
    hab svc load "devchef/a1-migration-data-minimal/0.0.1/$DATA_PACKAGE_RELEASE" &

    umask "$previous_umask"

    log_info "waiting for migration data to sync..."

    until [[ -f /etc/delivery/.a1-migration-data-ready ]]; do
        # print dots?
        ls /etc/delivery || true
        sleep 1
    done

    echo "done!"

    # Kill our supervisor from the migration package:
    # TODO (dan, 17 December 2018): with `core/hab-sup/0.70.0/20181205195836`
    # we are seeing a panic calling `hab sup term` here:
    # ```
    # hab sup term
    # thread 'main' panicked at 'LISTEN_GOSSIP should always have a value.', libcore/option.rs:1000:5
    # note: Run with `RUST_BACKTRACE=1` for a backtrace.
    # ```
    #
    # our workaround for now is to use kill instead of `hab sup term`
    # hab sup term # <- broken
    kill %1
    sleep 5
}
