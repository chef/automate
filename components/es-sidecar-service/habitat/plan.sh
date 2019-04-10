pkg_name=es-sidecar-service
pkg_description="A service providing common functionality to Automate's Elasticsearch consumers"
pkg_origin=chef
pkg_version="1.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/es-sidecar-service"
pkg_binds=(
  [automate-elasticsearch]="http-port http-host deprecated_external_es"
  [automate-es-gateway]="http-port http-host"
)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)
pkg_deps=(
  core/glibc
  chef/mlsa
)
pkg_build_deps=(
  core/gcc
)
pkg_bin_dirs=(bin)
pkg_scaffolding=chef/scaffolding-go
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)

do_prepare() {
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/a2/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/a2/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}
