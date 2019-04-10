pkg_name=automate-ui
# Ignore this package when bind data is compiled for deployment service:
chef_automate_dev_only_pkg=true
pkg_origin=chef
pkg_version="2.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("UNLICENSED")
pkg_svc_user="root" # so we can start nginx properly
pkg_deps=(
  core/nginx
)
pkg_build_deps=()
pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_description="devproxy for automate-ui"
pkg_upstream_url="https://www.chef.io/automate/"

do_unpack() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
