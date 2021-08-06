# shellcheck disable=SC2148
UPSTREAM_PKG_IDENT="chef/metricbeat/6.8.6"
pkg_name="automate-backend-metricbeat"
pkg_description="Wrapper package for chef/metricbeat"
pkg_origin="chef"
vendor_origin="chef"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
)

pkg_binds_optional=(
  #[database]="port superuser_name superuser_password"
)

pkg_binds=(
  #[elasticsearch]="http-port admin_username admin_password"
)

#pkg_version() {
  #cat "$PLAN_CONTEXT/../../VERSION"
#}

do_before() {
  #if [ ! -f "$PLAN_CONTEXT/../../VERSION" ]; then
   # exit_with "Cannot find VERSION file! You must enter the studio from the project's top-level directory." 56
  #fi
  update_pkg_version
}

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}

do_end() {
  return 0
}
