#stable channel

pkg_name=automate-builder-memcached
pkg_version=1.6.18
pkg_distname=memcached
pkg_dirname="${pkg_distname}-${pkg_version}"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache-2.0' 'BSD')
pkg_source="http://www.memcached.org/files/${pkg_distname}-${pkg_version}.tar.gz"
pkg_shasum=cbdd6ab8810649ac5d92fcd0fcb0ca931d8a9dbd0ad8cc575b47222eedd64158

pkg_deps=(
  core/bash
  core/glibc
  core/libevent
  core/openssl
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

