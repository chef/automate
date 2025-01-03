#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel


pkg_name="automate-builder-api-proxy"
pkg_origin="chef"
pkg_description="Wrapper package for habitat/builder-api-proxy"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_svc_user="root"

pkg_deps=(
  core/coreutils/9.4/20241017111752
  core/bash/5.1/20240105214248
  chef/mlsa/1.0.1/20240125084021
  chef/automate-platform-tools/0.1.0/20241212061203
  # We need to pin here to get a build from unstable
  habitat/builder-api-proxy/9639/20240722052815
)

pkg_build_deps=(
  core/gcc/9.5.0/20240105175314
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


