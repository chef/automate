# shellcheck disable=SC2148
# stable channel


pkg_name="automate-ha-elasticsidecar"
pkg_description="Sidecar for HA Opensearch"
pkg_origin="chef"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_version="0.1.0"
pkg_upstream_url="http://github.com/chef/automate/components/automate-backend-elasticsidecar"
pkg_deps=(
  core/bash
  chef/automate-openjdk
  chef/automate-ha-opensearch
)
pkg_build_deps=(
  core/make
  core/gcc
)

pkg_binds=(
  [opensearch]="http-port transport-port root-ca admin-pem admin-key admin_username admin_password"
)

pkg_lib_dirs=(lib)

do_before() {
  update_pkg_version
}

pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/automate-backend-elasticsidecar"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/automate-backend-elasticsidecar"
)

do_install() {
  do_default_install
}

do_end() {
  return 0
}
