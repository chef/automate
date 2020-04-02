pkg_name=automate-builder-memcached
pkg_version=1.5.19
pkg_distname=memcached
pkg_dirname="${pkg_distname}-${pkg_version}"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache-2.0' 'BSD')
pkg_source="http://www.memcached.org/files/${pkg_distname}-${pkg_version}.tar.gz"
pkg_shasum=3ddcdaa2d14d215f3111a7448b79c889c57618a26e97ad989581f1880a5a4be0
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
