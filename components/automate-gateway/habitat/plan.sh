pkg_name=automate-gateway
pkg_description="Automate API Gateway"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/automate-gateway"
pkg_version="0.1.0"
pkg_deps=(
  core/cacerts # communicate with license service over HTTPS
  core/curl
  core/glibc
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
pkg_binds_optional=(
  [authn-service]="port"
  [authz-service]="port"
  [config-mgmt-service]="port"
  [compliance-service]="port"
  [deployment-service]="port"
  [ingest-service]="port"
  [local-user-service]="port"
  [notifications-service]="port"
  [teams-service]="port"
  [license-control-service]="port"
  [secrets-service]="port"
  [event-service]="port"
  [nodemanager-service]="port"
)
pkg_exposes=(port)
pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
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

do_install() {
  scaffolding_go_install

  build_line "Packaging swagger documentation"
  mkdir -p "${pkg_prefix}/static/"
  cp -r "third_party/swagger-ui" "${pkg_prefix}/static/"
}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi;
}
