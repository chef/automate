pkg_name=config-mgmt-service
pkg_description="Configuration management API Service"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/config-mgmt-service"
pkg_version="0.1.0"
pkg_deps=(
  core/glibc
  core/grpcurl
  core/jq-static/1.6 # TODO(sr): drop version when erroneous jq-static/1.10 is gone
  chef/mlsa
)
pkg_build_deps=(
  core/gcc
)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_binds=(
  [automate-es-gateway]="http-port http-host"
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
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/automate/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi;
}
