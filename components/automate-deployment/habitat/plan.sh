#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name=deployment-service
pkg_description="Automate Deployment Service"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/automate-deployment"

pkg_deps=(
  # WARNING: DO NOT ALPHABETIZE or otherwise change the order of this
  # list without careful thought. The ordering of dependencies in this
  # list determines the order in which our PATH is constructed at
  # runtime by Habitat.
  # 
  # IF YOU UPDATE THIS PIN YOU MUST ALSO UPDATE .expeditor/create-manifest.rb
  core/hab/1.6.1243/20241227194506
  core/net-tools/2.10/20250529012700
  core/procps-ng/4.0.4/20250528173530
  core/util-linux/2.38.1/20250528182417
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  core/bash/5.2.21/20250528165700
  core/cacerts/2023.12.12/20250528165811 # fetching manifest over HTTPS
  core/certstrap/1.3.0/20250529005923
  core/coreutils/9.4/20250924120257
  core/findutils/4.9.0/20250528173106
  core/rsync/3.2.7/20250925044616
  core/tar/1.35/20250924121242
  chef/mlsa
  # deployment-service uses PostgreSQL client tools for backup and restore.
  core/postgresql14-client/14.17/20250924124514
)

pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
)

pkg_svc_user=root
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_no_platform=true # Don't inject automate platform scaffolding
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/automate-deployment"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_strip() {
   return 0
}


do_before() {
  do_default_before
  git config --global --add safe.directory /src
}
