pkg_name=compliance-service
pkg_description="Compliance API service"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="1.11.1"
pkg_upstream_url="http://github.com/chef/automate/components/compliance-service"
pkg_build_deps=(
  core/curl
  core/gcc
  core/jq-static
  core/tar
)
pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [automate-es-gateway]="http-port http-host"
  [secrets-service]="port"
  [event-service]="port"
  [authz-service]="port"
  [nodemanager-service]="port"
)
pkg_binds_optional=(
  [es-sidecar-service]="port"
  [authn-service]="port"
  [notifications-service]="port"
)
inspec_release="3.9.0/20190401200826"
pkg_deps=(
  core/bash
  core/glibc
  core/grpcurl              # Used in habitat/hooks/health_check
  core/jq-static            # Used in habitat/hooks/health_check
  ${local_platform_tools_origin:-chef}/automate-platform-tools
  # WARNING: Update with care. The chef/inspec is managed with Expeditor.

  # See .expeditor/update-inspec-version.sh for details
  chef/inspec/${inspec_release}
  chef/mlsa
)

if [[ -n "$AUTOMATE_OSS_BUILD" ]]; then
    echo "Not adding automate-compliance-profiles as AUTOMATE_OSS_BUILD is set!"
else
    # WARNING: chef/automate-compliance-profiles is managed by Expeditor
    # See .expeditor/update-compliance-profiles.sh for details
    pkg_deps+=(
        chef/automate-compliance-profiles/1.0.0/20190430094730
    )
fi

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
  "${scaffolding_go_import_path}/cmd/inspec_runner"
)

do_prepare() {
  # Injecting Version, SHA and Build Time to the go linker
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS=" -X ${scaffolding_go_base_path}/automate/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.BuildTime=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X main.EXECUTABLE_PATH=$(pkg_path_for chef/inspec)/bin/inspec"
  export GO_LDFLAGS
  build_line "Setting GO_LDFLAGS=${GO_LDFLAGS}"
}

do_install() {
  # Scaffolding go install callback
  scaffolding_go_install

  inspec_sem_version=$(awk -F  '/' '{print $1}' <<< ${inspec_release})
  build_line "Setting InSpec version ${inspec_sem_version}"
  sed -i "s/REPLACE-FROM-PLAN.SH/${inspec_sem_version}/" habitat/default.toml
  sed -i "s/REPLACE-FROM-PLAN.SH/${inspec_sem_version}/" ../../api/config/compliance/config_request.go

  build_line "Copying migration files"
  mkdir "${pkg_prefix}/migrations"
  cp -r dao/pgdb/migration/sql/* "${pkg_prefix}/migrations"

  build_line "Setting perms on inspec_runner"
  chown root: "${pkg_prefix}/bin/inspec_runner"
  chmod u+s "${pkg_prefix}/bin/inspec_runner"
}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi;
}
