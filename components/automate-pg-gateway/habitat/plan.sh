#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel

pkg_name=automate-pg-gateway
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="0.0.1"
pkg_deps=(
    core/bash/5.1/20240105214248
    core/netcat-openbsd/1.226/20240106172343
    core/jq-static/1.6/20240107004905
    core/haproxy/2.2.29/20240106203532
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
