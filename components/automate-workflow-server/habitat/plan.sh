pkg_name="automate-workflow-server"
pkg_origin="chef"
vendor_origin=${vendor_origin:-chef}
pkg_version=2.8.61
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_description="Chef Automate Workflow server"
pkg_upstream_url="https://www.chef.io/automate"

pkg_deps=(
  core/bash
  core/coreutils
  core/curl
  core/erlang18
  core/gawk
  core/gcc-libs
  core/grep
  core/git
  core/glibc
  core/openssh
  core/tzdata
  core/jq-static
  chef/mlsa

  # NOTE(ssd) 2019-04-03: Any in-repo dependencies added here MUST be
  # shared with automate-workflow-ctl until we either combine these
  # packages or remove all shared dependencies between the two
  # packages.
  ${local_platform_tools_origin:-chef}/automate-platform-tools

  core/bundler
  core/ruby
  "${vendor_origin}/automate-workflow-ctl"
)

pkg_build_deps=(
  core/gcc
  core/make
  core/which
  core/cacerts
  core/rsync
  core/wget
)

pkg_bin_dirs=(bin)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_exports=(
  [port]="api_port"
  [git_ssh_port]="ssh_git.port"
  [dc_token]="data_collector.token"
)

pkg_exposes=(port git_ssh_port)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

do_download() {
  return 0
}

do_unpack() {
  # Copy everything over to the cache path so we don't write out our compiled
  # deps into the working directory, but into the cache directory.
  mkdir -pv "$CACHE_PATH"
  cp -R "$SRC_PATH/"* "$CACHE_PATH"
}

do_prepare() {
  build_line "Setting ERL_FLAGS=-smp enable"
  export ERL_FLAGS="-smp enable"

  # Need this for enterprise_ctl to compile with an old version of rebar
  export PATH="$PATH:$CACHE_PATH"
  build_line "Setting PATH=$PATH"

  export RELX_OUTPUT_DIR="$pkg_prefix"
  build_line "RELX_OUTPUT_DIR=$pkg_prefix"

  git config --global http.sslCAInfo \
    "$(pkg_path_for cacerts)"/ssl/certs/cacert.pem

  # The `/usr/bin/env` path is hardcoded in the core/relx plan, so we'll add a
  # symlink.
  if [[ ! -r /usr/bin/env ]]; then
    ln -sv "$(pkg_path_for coreutils)/bin/env" /usr/bin/env
   _clean_env=true
  fi

  fix_interpreter "$CACHE_PATH/rebar" core/coreutils bin/env
  fix_interpreter "$CACHE_PATH/apps/enterprise_ctl/rebar" core/coreutils bin/env
}


do_build() {
  make --directory="$CACHE_PATH" distclean lean_rel with_patches
}

do_install() {
  # Create broken symlinks in the package that will be linked to the config
  # files. It's good to do this because, while you should not modify anything
  # in $pkg_prefix at runtime, sometimes the application requires that files
  # exist in that path. Creating broken symlinks signals that these files
  # should exist at these locations. The working links are created in the init
  # hook.
  rm "$pkg_prefix/delivery/releases/0.0.1/sys.config" \
    "$pkg_prefix/delivery/releases/0.0.1/vm.args"
  ln -sf "$pkg_svc_config_path/sys.config" "$pkg_prefix/delivery/releases/0.0.1/"
  ln -sf "$pkg_svc_config_path/vm.args" "$pkg_prefix/delivery/releases/0.0.1/"

  cp -R "$CACHE_PATH/schema" "$pkg_prefix"

  fix_interpreter "$pkg_prefix/delivery/bin/runner_ctl" core/coreutils bin/env
  fix_interpreter "$pkg_prefix/delivery/bin/enterprise_ctl" \
    core/coreutils bin/env
  fix_interpreter "$pkg_prefix/schema/wait-for-it.sh" \
    core/coreutils bin/env

  cp -a "${PLAN_CONTEXT}/../git_repo_template" "${pkg_prefix}/"

  fix_interpreter "$pkg_prefix/git_repo_template/hooks/update" \
    core/bash bin/bash

  # Set the path to curl in the git template hook we use to create git
  # repositories on disk.
  # NOTE: LOOK AT FIXING THIS since we control the code in A2
  # TODO: kill off the version in apps/delivery/priv/priv/git_repo_template/hooks/update"
  sed -i "s%{{CURL_PATH}}%$(pkg_path_for curl)%g" \
      "$pkg_prefix/git_repo_template/hooks/update"

  fix_interpreter "$pkg_prefix/git_repo_template/hooks/post-receive" core/coreutils bin/env

  # Create an workflow-ctl wrapper script for use inside a workflow-server container
  #
  # Why? Because automate-ctl collide with A2 automate-ctl command from deployment
  #
  # TODO @afiune if this binary is installed and not started as a service we should
  # print a nice message saying indicating to first start workflow.
  cat << EOF > "${pkg_prefix}/bin/workflow-ctl"
#!$(pkg_path_for bash)/bin/bash
source ${pkg_svc_config_path}/automate-ctl-config.sh
ctl_path=$(pkg_path_for ${vendor_origin}/automate-workflow-ctl)
export OMNIBUS_FILES="\${ctl_path}/lib"
export RUBYLIB="\${ctl_path}/lib"
export KNIFE_PATH="\${ctl_path}/bin/knife"
cd \$ctl_path
bundle exec bin/workflow-ctl "\$@"
EOF
  chmod +x "${pkg_prefix}/bin/workflow-ctl"
}
