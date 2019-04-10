pkg_name=automate-workflow-web
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')

pkg_deps=(
    core/coreutils
    chef/mlsa
)
pkg_build_deps=(
  core/make
  core/node/"$(cat "$PLAN_CONTEXT/../.nvmrc")"
  core/phantomjs
)
pkg_description="Web interface for Chef Automate Workflow"
pkg_upstream_url="https://www.chef.io/automate/"

do_download() {
  return 0
}

do_verify() {
  return 0
}

do_unpack() {
  # Copy the project files into the cache src path so we can have a clean
  # node_modules and only the files we need
  mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname"
  cp -R ../Makefile ../.babelrc ../.npmrc ../package.json ../tsconfig.json ../src ../config \
    "$HAB_CACHE_SRC_PATH/$pkg_dirname"
}

do_build() {
  # Install node modules
  make installnodedeps
  # Fix the interpreters of the binaries
  for b in node_modules/.bin/*; do
    fix_interpreter "$(readlink -f -n "$b")" core/coreutils bin/env
  done
  make build
}

do_install() {
  cp -R dist "$pkg_prefix"
}
