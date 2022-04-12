pkg_name=automate-supermarket-redis
pkg_origin="chef"
pkg_version="4.0.14"
pkg_description="Persistent key-value database, with built-in net interface"
pkg_upstream_url="http://redis.io/"
pkg_license=('Chef-MLSA')
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
vendor_origin="chef"
chef_automate_hab_binding_mode="relaxed"
pkg_svc_run="$(hab pkg path core/redis)/bin/redis-server ${pkg_svc_config_path}/redis.config"

pkg_deps=(
  "chef/mlsa"
  "${vendor_origin}/supermarket-redis/4.0.14/20220321050725"
)

# pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
# scaffolding_go_base_path=github.com/chef
# scaffolding_go_repo_name=supermarket
# scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/${pkg_name}"


pkg_exports=(
  [port]=port
)
pkg_exposes=(port)
do_build() {
  return 0
}

do_install() {
  # do_default_install
  # hab pkg install core/redis
  # install {{ pkg_path }}
  return 0
}

