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
  core/bash/5.1/20240105214248
  core/glibc/2.35/20240105171810
  core/libevent/2.1.12/20240106023556
  core/openssl11/1.1.1w/20240106015742
)


pkg_build_deps=(
  core/git/2.33.1/20240614092831
  core/gcc/9.5.0/20240105175314
  core/make/4.3/20240105222044
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

