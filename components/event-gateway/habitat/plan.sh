#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=event-gateway
pkg_description="Event Gateway for Chef Automate: provides NATS-protocol API to external clients"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/event-gateway"

pkg_deps=(
  core/bash
  chef/automate-platform-tools
  chef/mlsa
)

pkg_exports=(
  [port]=service.port
  [gateway_port]=service.gateway_port
  [host]=service.host
)
pkg_exposes=(
  port
  gateway_port
)
pkg_binds=(
  [authn-service]="port"
  [authz-service]="port"
  [event-service]="internal_messaging_gateway_port"
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
