#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel


pkg_name=compliance-service
pkg_description="Compliance API service"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="1.11.1"
pkg_upstream_url="http://github.com/chef/automate/components/compliance-service"
pkg_bin_dirs=(bin)
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
  [automate-es-gateway]="http-port"
  [secrets-service]="port"
  [event-service]="port"
  [authz-service]="port"
  [nodemanager-service]="port"
  [cereal-service]="port"
)
pkg_binds_optional=(
  [es-sidecar-service]="port"
  [authn-service]="port"
  [notifications-service]="port"
)
#Adding it to use compliance with firejail
pkg_svc_user=root
inspec_release="chef/inspec/4.56.61/20240809111842"
pkg_deps=(
  core/coreutils/8.32/20240105213308
  chef/automate-platform-tools/0.1.0/20241212061203
  "${inspec_release}"
  chef/mlsa/1.0.1/20240125084021
  core/grpcurl/1.8.5/20240109144108              # Used in habitat/hooks/health_check
  core/jq-static/1.6/20240107004905           # Used in habitat/hooks/health_check
  core/bash/5.1/20240105214248
  core/firejail/0.9.72/20240109161319
)

if [[ -n "$AUTOMATE_OSS_BUILD" ]]; then
  echo "Not adding automate-compliance-profiles as AUTOMATE_OSS_BUILD is set!"
else
  # WARNING: chef/automate-compliance-profiles is managed by Expeditor
  # See .expeditor/update-compliance-profiles.sh for details
  pkg_deps+=(
      chef/automate-compliance-profiles/1.0.0/20250609122212
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
  do_default_prepare
   
  GO_LDFLAGS="${GO_LDFLAGS} -X main.EXECUTABLE_PATH=$(pkg_path_for chef/inspec)/bin/inspec"
 export GO_LDFLAGS
  
}

do_install() {
  do_default_install

  echo $HOME

  inspec_sem_version=$(awk -F  '/' '{print $3}' <<< ${inspec_release})
  build_line "Setting InSpec version ${inspec_sem_version}"
  sed -i "s/REPLACE-FROM-PLAN.SH/${inspec_sem_version}/" habitat/default.toml

  build_line "Copying migration files"
  mkdir "${pkg_prefix}/migrations"
  cp -r dao/pgdb/migration/sql/* "${pkg_prefix}/migrations"

  build_line "Setting perms on inspec_runner"
  chown root: "${pkg_prefix}/bin/inspec_runner"
  chmod u+s "${pkg_prefix}/bin/inspec_runner"

  mkdir -p "${pkg_prefix}/data/firejail"

  cp -r firejail/* "${pkg_prefix}/data/firejail"


}

do_strip() {
  if [[ "${CHEF_DEV_ENVIRONMENT}" != "true" ]]; then
    do_default_strip
  fi
}

do_before() {
  do_default_before
  git config --global --add safe.directory /src
}