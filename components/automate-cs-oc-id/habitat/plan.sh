#shellcheck disable=SC2148

UPSTREAM_PKG_IDENT="chef/automate-cs-oc-id"
pkg_name="automate-cs-oc-id"
pkg_description="Wrapper package for chef/oc_id"
pkg_origin="chef"
# WARNING: Version managed by .expeditor/update_chef_server.sh
pkg_version="15.4.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=("${UPSTREAM_PKG_IDENT}")
pkg_svc_user=root
pkg_svc_group=root
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/oc_id/15.4.0/20230105061030"
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
 return 0
}

do_end() {
 return 0
}