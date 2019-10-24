#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=ingest-service
pkg_description="Ingestion service of Chef data"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/ingest-service"
pkg_version="0.1.0"
pkg_binds=(
  [automate-es-gateway]="http-port http-host"
  [authz-service]="port"
  [event-service]="port"
  [nodemanager-service]="port"
  # TODO(ssd) 2019-05-02: We could consider making these optional
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [cereal-service]="port"
)
pkg_binds_optional=(
  [es-sidecar-service]="port"
)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)
pkg_deps=(
  core/grpcurl
  core/jq-static
  chef/automate-platform-tools
  chef/mlsa
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
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi
}
