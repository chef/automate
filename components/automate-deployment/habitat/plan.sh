#shellcheck disable=SC2034
#shellcheck disable=SC2154

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
  core/hab/0.69.0/20181127182011
  core/net-tools
  core/procps-ng
  core/util-linux
  chef/automate-platform-tools
  core/bash
  core/cacerts # fetching manifest over HTTPS
  core/certstrap
  core/coreutils
  core/findutils
  core/rsync
  core/tar
  chef/mlsa
  # deployment-service uses the postgres11 client to backup/restore postgres.
  # we need pg11 because the ha backend uses postgres 11
  core/postgresql11-client
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
