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
  core/hab/0.82.0/20190605214032
  core/net-tools
  core/procps-ng
  core/util-linux
  chef/automate-platform-tools
  core/bash
  core/cacerts # fetching manifest over HTTPS
  core/certstrap
  core/coreutils
  core/findutils
  core/glibc
  core/rsync
  core/tar
  chef/mlsa
)
pkg_build_deps=(
  core/gcc
  core/git
)
pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
)
pkg_svc_user=root
pkg_scaffolding=chef/scaffolding-go
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/automate-deployment"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_strip() {
    return 0;
}

do_prepare() {
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/automate/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}
