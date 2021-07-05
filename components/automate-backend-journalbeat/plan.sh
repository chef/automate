UPSTREAM_PKG_IDENT="chef/journalbeat/6.8.6"
pkg_name=journalbeat-service
pkg_description="Wrapper package for chef/journalbeat"
pkg_origin="chef"
pkg_version="0.1.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="http://github.com/chef/automate/components/journalbeat-service"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_svc_user=root
pkg_svc_group=root
pkg_deps=(
  chef/mlsa
  core/bash
  "${UPSTREAM_PKG_IDENT}"
)

pkg_binds=(
  [automate-elasticsearch]="http-port"
)


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
