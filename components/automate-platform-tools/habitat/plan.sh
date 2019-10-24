#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=automate-platform-tools
pkg_description="Small command-line utilities for Chef Automate"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  core/glibc
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_no_platform=true # Don't inject automate platform scaffolding
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/secrets-helper"
  "${scaffolding_go_import_path}/cmd/service-health"
  "${scaffolding_go_import_path}/cmd/pg-helper"
  "${scaffolding_go_import_path}/cmd/render-template"
)

do_strip() {
  return 0
}
