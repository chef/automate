#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=trial-license-service
pkg_origin=chef
pkg_description="A2 trial license service"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/trial-license-service"
pkg_deps=(
  core/curl # health check
)
pkg_exports=(
  [port]=service.port # default service is http
)
pkg_exposes=(
  port
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_no_platform=true # Don't inject automate platform scaffolding
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_strip() {
  return 0
}
