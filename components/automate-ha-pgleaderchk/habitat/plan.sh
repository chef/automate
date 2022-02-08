# shellcheck disable=SC2148
#stable channel

pkg_name=automate-ha-pgleaderchk
binary_name=pgleaderchk
pkg_origin=chef
pkg_version="0.1.0"
pkg_repo=automate
pkg_description="Automate Backend PostreSQL leader check"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/automate-ha-pgleaderchk"
pkg_deps=(
  core/bash
  core/curl
  core/jq-static
  chef/mlsa
)
pkg_build_deps=(
  core/gcc
)
pkg_exports=(
  [port]=httpd.port
)
pkg_binds_optional=(
  [database]="port"
  [database]="ssl"
)
pkg_exposes=(port)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_install() {
  build_line "Overriding Install process"
   do_default_install
}

do_strip() {
  return 0
}
