#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=event-feed-service
pkg_description="Event Feed API Service"
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/event-feed-service"
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)

pkg_binds=(
  [automate-es-gateway]="http-port http-host"
  [cereal-service]="port"
)
pkg_binds_optional=(
  [es-sidecar-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)
