pkg_repo=a2
pkg_name=notifications-service
pkg_origin=chef
pkg_version=$(cat "$PLAN_CONTEXT/../VERSION")
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate"

pkg_deps=(
  core/coreutils
  # bash is required by distillery
  core/bash
  chef/mlsa
  ${local_platform_tools_origin:-chef}/automate-platform-tools
)
pkg_build_deps=(
  core/git
  core/erlang
  # NOTE(ssd) 2019-07-03: PIN PIN PIN
  #
  # elixir 1.9.0 shipped with a number of changes to how releases
  # work. This appears to have broken the build. Pinning until we can
  # sort out the required changes.
  core/elixir/1.8.0
  core/glibc
)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [secrets-service]="port"
)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)
pkg_description="Chef Automate Notifications Service"
pkg_upstream_url="https://www.chef.io/automate/"
pkg_bin_dirs=(bin)
pkg_lib_dirs=(lib)

# The service files and directories
# NOTE: this has nothing to do with habitat. It is used for
#       determining when a build is stale in the studiorc
pkg_srcs=(
  server/lib
  server/priv
  server/rel
  server/config.exs
  server/Makefile
  server/mix.exs
  server/mix.lock

  # TODO: VERSION should be in this list
)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"

do_unpack() {
  mkdir -p "${CACHE_PATH}/server"
  pushd "${PLAN_CONTEXT}/.." > /dev/null
    ls
    cp -av  ${pkg_srcs[@]} "${CACHE_PATH}/server"
  popd > /dev/null
  cp "${PLAN_CONTEXT}/../VERSION" "${CACHE_PATH}/"
}

do_prepare() {
  localedef -i en_US -f UTF-8 en_US.UTF-8
  export LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

  export MIX_HOME="${CACHE_PATH}/.mix"
  export HEX_HOME="${CACHE_PATH}/.hex"
  mix local.hex --force
  mix local.rebar --force
  fix_interpreter "${MIX_HOME}/*" core/coreutils bin/env
}

do_build() {
  pushd "${CACHE_PATH}/server" > /dev/null
    MIX_ENV=habitat mix do deps.get, release
  popd > /dev/null
  TARGET="${CACHE_PATH}/server/_habitat_build/habitat/rel/notifications"

  # Distillery seems to be chmoding stuff and not respecting our umask
  # Here's the bit of code in distillery that's causing problems:
  # https://github.com/bitwalker/distillery/blob/b695cfb3899ef1181d4af43fe9db54a851516277/lib/mix/lib/releases/assembler.ex#L249-L260
  find "${TARGET}" -xdev -perm -0002 -type f -print 2>/dev/null | xargs -I '{}' chmod go-w '{}'

  fix_interpreter "${TARGET}/releases/*/*.sh" core/coreutils bin/env
  fix_interpreter "${TARGET}/releases/*/libexec/*.sh" core/coreutils bin/env
  fix_interpreter "${TARGET}/releases/*/libexec/commands/*.sh" core/coreutils bin/env
  fix_interpreter "${TARGET}/bin/*.sh" core/coreutils bin/env
  fix_interpreter "${TARGET}/bin/notifications" core/coreutils bin/env
}

do_install() {
  cp -av "${CACHE_PATH}/server/_habitat_build/habitat/rel/notifications/." "${pkg_prefix}"
}
