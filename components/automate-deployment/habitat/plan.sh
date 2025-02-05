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
  core/hab/1.6.1205/20241107140309
  core/net-tools/1.60/20240107000437
  core/procps-ng/3.3.17/20240105213209
  core/util-linux/2.37/20240105222503
  #"${local_platform_tools_origin:-chef}/automate-platform-tools"
  core/bash/5.1/20240105214248
  core/cacerts/2021.10.26/20240105224256 # fetching manifest over HTTPS
  core/certstrap/v1.2.0/20240107042707
  core/coreutils/8.32/20240105213308
  core/findutils/4.9.0/20240105220908
  core/rsync/3.2.3/20240107034222
  core/tar/1.35/20240105214549
  chef/mlsa/1.0.1/20240125084021
  "${local_platform_tools_origin:-jashaik}/automate-platform-tools"
  # deployment-service uses the postgres11 client to backup/restore postgresql.
  # we need pg11 because the ha backend uses postgres 11
  core/postgresql13-client/13.18/20241203070721
)

pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
)

pkg_svc_user=root
pkg_scaffolding="${local_scaffolding_origin:-jashaik}/automate-scaffolding-go"
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
