# shellcheck disable=SC2148
# stable channel

UPSTREAM_PKG_IDENT="core/postgresql13/13.5"
pkg_name="automate-ha-postgresql"
pkg_description="Wrapper package for core/postgresql13"
pkg_origin="chef"
vendor_origin="chef"
pkg_version="13.5.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"

pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
)

pkg_exports=(
  [port]=port
  [superuser_name]=superuser.name
  [superuser_password]=superuser.password
  [ssl]=ssl.enable
)

pkg_exposes=(port)

do_before() {
  update_pkg_version
}

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  $(pkg_path_for core/bash)/bin/bash $PLAN_CONTEXT/cert.sh
}

do_after() {
  build_line "Replacing UPSTREAM_PKG_IDENT with ${UPSTREAM_PKG_IDENT} in ${pkg_prefix}/config/functions.sh"
  sed -i -e "s,UPSTREAM_PKG_IDENT,${UPSTREAM_PKG_IDENT},g" "${pkg_prefix}/config/functions.sh"
}

do_end() {
  return 0
}
