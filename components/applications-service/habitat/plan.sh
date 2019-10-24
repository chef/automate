#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=applications-service
pkg_description="Applications API Service"
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/applications-service"
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
  [metrics-port]=service.metrics_port
)
pkg_exposes=(
  port
  metrics-port
)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [event-service]="internal_messaging_port cluster_id"
  [cereal-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
  "${scaffolding_go_import_path}/cmd/applications-publisher"
  "${scaffolding_go_import_path}/cmd/applications-load-gen"
)

do_install() {
  do_default_install

  build_line "Copying schema sql files"
  mkdir "${pkg_prefix}/schema"
  cp -r pkg/storage/postgres/schema/sql/* "${pkg_prefix}/schema"
}
