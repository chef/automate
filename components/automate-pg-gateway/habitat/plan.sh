#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name=automate-pg-gateway
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="0.0.1"
pkg_deps=(
    core/bash
    core/netcat-openbsd
    core/jq-static
    core/haproxy
)

chef_automate_hab_binding_mode="relaxed"
pkg_exposes=(port)
pkg_exports=(
    [port]=service.port
)
pkg_binds=(
  [automate-postgresql]="port"
)
pkg_description="PostgreSQL Gateway for Chef Automate"
pkg_upstream_url="https://www.chef.io/automate"

do_download() {
  return 0
}

do_build() {
    return 0
}

do_install() {
    return 0
}

do_strip() {
  return 0
}

