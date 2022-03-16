# shellcheck disable=SC2148
# stable channel

UPSTREAM_PKG_IDENT="chef/metricbeat/6.8.23"
pkg_name="automate-ha-metricbeat"
pkg_description="Wrapper package for chef/metricbeat"
pkg_origin="chef"
pkg_version="6.8.23"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
)

pkg_binds_optional=(
  [database]="port superuser_name superuser_password"
)

pkg_binds=(
  [elasticsearch]="http-port admin_username admin_password"
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

do_end() {
  return 0
}
