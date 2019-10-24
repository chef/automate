#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=es-sidecar-service
pkg_description="A service providing common functionality to Automate's Elasticsearch consumers"
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/es-sidecar-service"
pkg_binds=(
  [automate-elasticsearch]="deprecated_external_es"
  [automate-es-gateway]="http-port http-host"
)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)
pkg_deps=(
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
