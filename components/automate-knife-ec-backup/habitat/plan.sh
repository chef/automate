# shellcheck disable=SC2148
UPSTREAM_PKG_IDENT="chef/knife-ec-backup/3.0.0"
pkg_name="automate-knife-ec-backup"
pkg_description="Wrapper package for chef/knife-ec-backup"
pkg_origin="chef"
pkg_version="0.1.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_svc_user=root
pkg_svc_group=root
pkg_deps=(
  chef/mlsa
  "${UPSTREAM_PKG_IDENT}"
)


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
  hab install "chef/knife-ec-backup" -bf
  return 0
}

do_end() {
 return 0
}
