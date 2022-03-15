# shellcheck disable=SC2148
# stable channel

UPSTREAM_PKG_IDENT="chef/journalbeat/6.8.23"
pkg_name="automate-ha-journalbeat"
pkg_description="Wrapper package for chef/journalbeat"
pkg_origin="chef"
pkg_version="6.8.23"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_svc_user=root
pkg_svc_group=root
pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
)

pkg_binds=(
  [elasticsearch]="http-port root-ca admin_username admin_password"
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
