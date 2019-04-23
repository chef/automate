pkg_name=automate-workflow-ctl
pkg_origin=chef
pkg_version=1.0.0
pkg_maintainer="The Chef Automate Maintainers <support@chef.io>"
pkg_license=('Chef-MLSA')
vendor_origin=${vendor_origin:-"chef"}

pkg_deps=(
  core/coreutils
  core/ruby
  core/bundler
  # NOTE(ssd) 2019-04-03: This dependency isn't needed, but we want to
  # make sure that this package always gets built whenever
  # automate-workflow-server gets built so we have to share all
  # in-repo dependencies.
  ${local_platform_tools_origin:-chef}/automate-platform-tools
)
pkg_build_deps=(
  core/gcc
  core/git
  core/make
)
pkg_lib_dirs=(lib)
pkg_bin_dirs=(bin)
pkg_description="Command line tool to control Chef Automate"
pkg_upstream_url="https://www.chef.io/automate/"

CTL_BIN_NAME="workflow-ctl" # the binary/script inside the package

do_download() {
  return 0
}

do_verify() {
  return 0
}

do_unpack() {
  mkdir -pv "$HAB_CACHE_SRC_PATH/$pkg_dirname"
  cp -R "$PLAN_CONTEXT"/../* "$HAB_CACHE_SRC_PATH/$pkg_dirname"
}

do_prepare() {
  export BUNDLE_SILENCE_ROOT_WARNING=1 GEM_PATH
  build_line "Setting BUNDLE_SILENCE_ROOT_WARNING=$BUNDLE_SILENCE_ROOT_WARNING"
  GEM_PATH="$(pkg_path_for core/bundler)"
  build_line "Setting GEM_PATH=$GEM_PATH"
}

do_build() {
  return 0
}

do_install() {
  cp -R lib/* "$pkg_prefix/lib"
  install -m 0755 "bin/${CTL_BIN_NAME}" "$pkg_prefix/bin/${CTL_BIN_NAME}"
  install -m 0644 Gemfile.lock "$pkg_prefix/Gemfile.lock"
  install -m 0644 Gemfile "$pkg_prefix/Gemfile"
  cd $pkg_prefix
  bundle install \
    --jobs "$(nproc)" \
    --path "$pkg_prefix/lib" \
    --retry 5 \
    --standalone \
    --without development test \
    --binstubs

  fix_interpreter "$pkg_prefix/bin/*" core/coreutils bin/env
  fix_interpreter "$pkg_prefix/bin/*" core/ruby bin/ruby
  fix_interpreter "$pkg_prefix/bin/knife" core/coreutils bin/env

  build_line "Creating bundler config"
  mkdir -p "$pkg_prefix/.bundle"
  # This bundle config is required since we use bundler at run
  cat >"$pkg_prefix/.bundle/config"<<EOF
---
BUNDLE_PATH: "${pkg_prefix}/lib"
BUNDLE_BIN: "bin"
BUNDLE_WITHOUT: "development:test"
EOF
}

do_strip() {
  return 0
}
