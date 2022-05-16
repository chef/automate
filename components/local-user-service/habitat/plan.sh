#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name=local-user-service
pkg_description="GRPC API that wraps dex's local user service"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/local-user-service"
pkg_deps=(
  core/bash
  chef/mlsa
)
pkg_exports=(
  [port]=service.port # default service is grpc
)
pkg_exposes=(
  port
)
pkg_binds=(
  [automate-dex]="grpc-host grpc-port port"
  [teams-service]="port"
  [authz-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/users"
  "${scaffolding_go_import_path}/cmd/users-health"
)

do_strip() {
  return 0
}
