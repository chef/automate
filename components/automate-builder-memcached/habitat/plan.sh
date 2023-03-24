#stable channel

pkg_name=automate-builder-memcached
pkg_version=1.6.12
pkg_distname=memcached
pkg_dirname="${pkg_distname}-${pkg_version}"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache-2.0' 'BSD')
pkg_source="http://www.memcached.org/files/${pkg_distname}-${pkg_version}.tar.gz"
pkg_shasum=f291a35f82ef9756ed1d952879ef5f4be870f932bdfcb2ab61356609abf82346
pkg_deps=(
  core/bash
  core/glibc
  core/libevent
  core/openssl11
)
pkg_build_deps=(
  core/git
  core/gcc
  core/make
)
pkg_bin_dirs=(bin)
pkg_include_dirs=(include)
pkg_lib_dirs=(lib)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"

do_build() {
  ./configure \
    --prefix="${pkg_prefix}" \
    --enable-tls
  make
}

# Tests requires various perl modules
# do_check() {
#   make test
# }
