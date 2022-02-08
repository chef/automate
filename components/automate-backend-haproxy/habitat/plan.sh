# shellcheck disable=SC2148
# stable channel

UPSTREAM_PKG_IDENT="core/haproxy"
pkg_name="automate-ha-haproxy"
pkg_description="Wrapper package for core/haproxy"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_version="2.2.14"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_deps=(
  chef/mlsa
  core/bash
  core/netcat-openbsd
  "${UPSTREAM_PKG_IDENT}"
)


pkg_exports=(
  [port]=front-end.port
  [status-port]=status.port
)

pkg_exposes=(port status-port)

pkg_binds_optional=(
  [database]="port superuser_name"
  [pgleaderchk]="port"
)

do_before() {
  update_pkg_version
}

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
