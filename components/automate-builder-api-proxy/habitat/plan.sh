#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name="automate-builder-api-proxy"
pkg_origin="chef"
pkg_description="Wrapper package for habitat/builder-api-proxy"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_svc_user="root"
pkg_deps=(
  core/runit
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  "habitat/builder-api-proxy"
)

pkg_build_deps=(
  core/gcc
)

pkg_binds=(
  [automate-builder-api]="http-port"
)

pkg_exports=(
  [port]=service.port
)

pkg_exposes=(port)

pkg_bin_dirs=(bin)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"

do_unpack() {
    return 0
}
do_build(){
    return 0
}
do_install() {
    mkdir -p "${pkg_prefix}/config/"
    proxypath="$(pkg_path_for habitat/builder-api-proxy)"
    proxyrel="${proxypath##*/}"
    sed -r "s/BUILDER_API_PROXY_RELEASE/${proxyrel}/g" support/index.html > "${pkg_prefix}/config/index.html"

    return 0
}
do_strip() {
    return 0
}
