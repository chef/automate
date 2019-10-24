#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=nodemanager-service
pkg_description="Nodemanager Service"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="1.0.0"
pkg_upstream_url="http://github.com/chef/automate/components/nodemanager"
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  core/bash
)
pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [secrets-service]="port"
  [event-service]="port"
  [cereal-service]="port"
  [authz-service]="port"
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

do_install() {
  do_default_install

  build_line "Copying migration files"
  mkdir "${pkg_prefix}/migrations"
  cp -r pgdb/migration/sql/* "${pkg_prefix}/migrations"
}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi
}
