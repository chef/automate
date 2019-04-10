pkg_name=data-feed-service
pkg_description="Data Feed Service"
pkg_origin=chef
pkg_version=1.0.0
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/data-feed-service"
pkg_deps=(
  core/bash
  core/glibc
  ${local_platform_tools_origin:-chef}/automate-platform-tools
  chef/mlsa
)
pkg_build_deps=(
  core/git # for ref in version
  core/gcc
)
pkg_exports=(
  [port]=service.port # default service is grpc
  [host]=service.host
)
pkg_exposes=(
  port
)
pkg_binds=(
  [notifications-service]="port"
  [secrets-service]="port"
  [config-mgmt-service]="port"
)
pkg_bin_dirs=(bin)
pkg_scaffolding=chef/scaffolding-go
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_build_tags=(prod)
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_strip() {
    return 0;
}

do_prepare() {
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/a2/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}

