# shellcheck disable=SC2148
UPSTREAM_PKG_IDENT="chef/kibana-odfe/0.10.0.4"
pkg_name="automate-backend-kibana"
pkg_description="Wrapper package for chef/kibana"
pkg_origin="chef"
vendor_origin="chef"
pkg_version="6.8.6"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_svc_user="root"
pkg_deps=(
  chef/mlsa
  core/bash
  core/openssl
  "${UPSTREAM_PKG_IDENT}"
)

pkg_binds=(
  [elasticsearch]="http-port root-ca admin-key admin-pem admin_username admin_password"
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
