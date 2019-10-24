#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=license-control-service
pkg_description="A2 license control service"
pkg_origin=chef
pkg_version="1.0.0"
pkg_bin_dirs=(bin)
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_deps=(
  chef/mlsa
  chef/automate-platform-tools
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_exposes=(port)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_install() {
  do_default_install

  build_line "Copying migration files"
  cp -r migrations "${pkg_prefix}/migrations"
}
