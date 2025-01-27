#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel


pkg_name=pg-sidecar-service
pkg_description="A service providing common functionality to Automate's Postgres consumers"
pkg_origin=chef
pkg_version="0.0.1"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/pg-sidecar-service"
pkg_binds=(
  [automate-pg-gateway]="port"
)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)
pkg_deps=(
  chef/mlsa/1.0.1/20240125084021
  chef/automate-platform-tools
  # Pin postgresql, sqitch_pg, and postgresql client until automate-postgresql
  # is updated.
  core/postgresql13/13.18/20241203070217 # for psql and pg_dump
  core/postgresql13-client/13.18/20241203070721
  core/sqitch_pg/3.15.0/20240614112821
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_before() {
  do_default_before
  git config --global --add safe.directory /src
}


