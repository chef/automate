#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name="automate-builder-api"
pkg_description="Wrapper package for habitat/builder-api"
pkg_origin="chef"
pkg_version="0.1.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  core/bash
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  "jaym/builder-api"
)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [automate-builder-memcached]="port"
)

pkg_exports=(
  [http-port]=service.port
)

pkg_exposes=(http-port)

pkg_bin_dirs=(bin)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"

do_unpack() {
    return 0
}
do_build(){
    return 0
}
do_install() {
    return 0
}
do_strip() {
    return 0
}
