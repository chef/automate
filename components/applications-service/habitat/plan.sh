#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name=applications-service
pkg_description="Applications API Service"
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/applications-service"
pkg_deps=(
  core/glibc
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_build_deps=(
  core/gcc
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
  [metrics-port]=service.metrics_port
)
pkg_exposes=(
  port
  metrics-port
)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [event-service]="internal_messaging_port cluster_id"
)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
  "${scaffolding_go_import_path}/cmd/applications-publisher"
)

do_prepare() {
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/a2/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}

do_install() {
  # Go scaffolding install callback
  scaffolding_go_install

  build_line "Copying schema sql files"
  mkdir "${pkg_prefix}/schema"
  cp -r pkg/storage/postgres/schema/sql/* "${pkg_prefix}/schema"
}
