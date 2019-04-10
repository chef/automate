#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name="automate-cs-oc-bifrost"
pkg_description="Wrapper package for chef/oc_bifrost"
pkg_origin="chef"
pkg_version="12.19.31"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  "${vendor_origin}/oc_bifrost/12.19.31/20190307135121"
)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
)

pkg_exposes=(http-port)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
