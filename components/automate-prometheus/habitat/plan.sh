# shellcheck disable=SC2034
# shellcheck disable=SC2039

pkg_name=automate-prometheus
pkg_description="metrics collection for Automate 2.0"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache 2')
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  core/prometheus
  core/bash
)

pkg_exports=(
  [http-port]=http.port
)

chef_automate_hab_binding_mode="relaxed"

pkg_binds_optional=(
  [automate-gateway]="port"
  [applications-service]="metrics-port"
)

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
