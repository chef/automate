#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name=session-service
pkg_description="A2 session service"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/session-service"
pkg_deps=(
  core/bash
  core/curl # health_check hook
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_exports=(
  [port]=service.port # default service is http
  [grpc-port]=service.grpc_port
)
pkg_exposes=(
  port
  grpc-port
)
pkg_binds=(
  [automate-dex]="port"
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

do_strip() {
  return 0
}

do_install() {
  do_default_install

  build_line "Copying migration files"
  cp -r migration/sql "${pkg_prefix}/migrations"
}

