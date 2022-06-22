#stable channel

pkg_name=automate-cds
pkg_description="Automate context delivery service proxy"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/automate-cds"
pkg_deps=(
  core/bash
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  chef/mlsa
)
pkg_exports=(
  [port]=service.port # default service is grpc
  [host]=service.host
)
pkg_binds=(
  [compliance-service]="port"
)
pkg_exposes=(port)

pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_build_tags=(prod)
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

