# shellcheck disable=SC2148
# stable channel

UPSTREAM_PKG_IDENT="core/curator"
pkg_name="automate-ha-curator"
pkg_description="Wrapper package for core/curator"
pkg_origin="chef"
vendor_origin="chef"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_svc_user="root"
pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
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
