# shellcheck disable=SC2148
UPSTREAM_PKG_IDENT="chef/elasticsearch-odfe/0.10.1.2"
pkg_name="automate-backend-elasticsearch"
pkg_description="Wrapper package for elasticsearch-odfe"
pkg_origin="chef"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  chef/mlsa
  core/bash
  core/procps-ng
  "${UPSTREAM_PKG_IDENT}"
)

pkg_version() {
  cat "$PLAN_CONTEXT/../../VERSION"
}

pkg_lib_dirs=(lib)

pkg_exports=(
  [http-port]=es_yaml.http.port
  [transport-port]=es_yaml.transport.tcp.port
  [root-ca]=opendistro_ssl.rootCA
  [admin-pem]=opendistro_ssl.admin_cert
  [admin-key]=opendistro_ssl.admin_key
  [admin_username]=opendistro_auth.admin_username
  [admin_password]=opendistro_auth.admin_password
  [dashboard_username]=opendistro_auth.dashboard_username
  [dashboard_password]=opendistro_auth.dashboard_password
)
pkg_exposes=(http-port transport-port)

do_before() {
  if [ ! -f "$PLAN_CONTEXT/../../VERSION" ]; then
    exit_with "Cannot find VERSION file! You must enter the studio from the project's top-level directory." 56
  fi
  update_pkg_version
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
