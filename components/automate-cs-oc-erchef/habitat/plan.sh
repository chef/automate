#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-cs-oc-erchef"
pkg_description="Wrapper package for chef/oc_erchef"
pkg_origin="chef"
# WARNING: Version managed by .expeditor/update_chef_server.sh
pkg_version="14.13.42"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  #core/runit/2.1.2/20200403133549
  core/runit
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/oc_erchef/14.13.42/20220228221324"
)

pkg_build_deps=(
  core/gcc
)

pkg_binds=(
  [automate-cs-oc-bifrost]="http-port"
  [automate-es-gateway]="http-port"
  [automate-cs-bookshelf]="http-port"
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)

pkg_binds_optional=(
  [automate-gateway]="port"
)

pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
)

pkg_exposes=(http-port)

pkg_bin_dirs=(bin)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
automate_scaffolding_include_templates=(sqerl.config)

scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/init-es"
)

chef_automate_hab_binding_mode="relaxed"

