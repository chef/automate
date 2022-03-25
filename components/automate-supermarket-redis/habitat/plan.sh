pkg_name="automate-supermarket-redis"
pkg_origin="chef"
pkg_version="4.0.14"
pkg_description="Persistent key-value database, with built-in net interface"
pkg_upstream_url="http://redis.io/"
pkg_license=('Apache-2.0')
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
pkg_svc_user="root"
pkg_svc_group="root"
pkg_svc_run="redis-server ${pkg_svc_config_path}/redis.config"
vendor_origin="chef"
chef_automate_hab_binding_mode="relaxed"

pkg_exports=(
  [port]=port
)
pkg_exposes=(port)
pkg_deps=(  
  "${vendor_origin}/supermarket-redis/4.0.14/20220321050725"
)
do_build() {
  return 0
}

do_install() {
  return 0
}

