pkg_name="automate-supermarket-redis"
pkg_origin="chef"
pkg_version="4.0.14"
pkg_description="Persistent key-value database, with built-in net interface"
pkg_upstream_url="http://redis.io/"
pkg_license=('Apache-2.0')
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
pkg_svc_user="root"
pkg_svc_group="root"
vendor_origin="chef"
pkg_bin_dirs=(bin)
chef_automate_hab_binding_mode="relaxed"
pkg_deps=(
  core/redis   
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"


pkg_exports=(
  [port]=port
)
pkg_exposes=(port)
do_build() {
  return 0
}

do_install() {
  # hab pkg install core/redis
  # install {{ pkg_path }}
  return 0
}

