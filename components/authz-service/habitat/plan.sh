#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=authz-service
pkg_description="A2 authorization service"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/authz-service"
pkg_deps=(
  core/bash
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  chef/mlsa
)
pkg_exports=(
  [port]=service.port # default service is grpc
)
pkg_exposes=(
  port
)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [cereal-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_build_tags=(prod)
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_strip() {
  return 0
}

do_install() {
  do_default_install

  build_line "Copying migration files"
  cp -r storage/postgres/migration/sql "${pkg_prefix}/migrations"
  cp -r storage/postgres/datamigration/sql "${pkg_prefix}/data-migrations"
}
