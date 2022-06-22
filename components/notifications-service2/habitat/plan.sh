#stable channel

# NOTE/TODO: remove this when we cut over the packages
chef_automate_dev_only_pkg=true

pkg_name=notifications-service
pkg_description="Chef Automate Notifications Service"
pkg_origin=chef
pkg_version="2.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate"


pkg_deps=(
  chef/mlsa
  ${local_platform_tools_origin:-chef}/automate-platform-tools
)

pkg_exports=(
  [port]=service.port
)

pkg_exposes=(port)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [secrets-service]="port"
)
pkg_bin_dirs=(bin)
pkg_lib_dirs=(lib)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate

# NOTE/TODO: This references the temporary name/path components/notifications-service2
# when we cut over, this needs to be changed to the commented version below
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/notifications-service2"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

##  scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
##  scaffolding_go_binary_list=(
##    "${scaffolding_go_import_path}/cmd/${pkg_name}"
##  )

do_install() {
  do_default_install

  build_line "Copying schema sql files"
  mkdir "${pkg_prefix}/schema"
  cp -r pkg/storage/postgres/schema/sql/* "${pkg_prefix}/schema"
}

do_strip() {
  return 0
}

