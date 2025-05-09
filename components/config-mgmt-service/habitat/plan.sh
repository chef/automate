#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel


pkg_name=config-mgmt-service
pkg_description="Configuration management API Service"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/config-mgmt-service"
pkg_version="0.1.0"
pkg_deps=(
  core/grpcurl
  core/jq-static
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_binds=(
  [automate-es-gateway]="http-port"
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)


do_install() {
  do_default_install

  build_line "Copying schema sql files"
  mkdir "${pkg_prefix}/schema"
  cp -r backend/postgres/schema/sql/* "${pkg_prefix}/schema"
}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi
}


do_before() {
  do_default_before
  git config --global --add safe.directory /src
}