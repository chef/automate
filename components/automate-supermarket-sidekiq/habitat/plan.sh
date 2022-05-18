pkg_name=automate-supermarket-redis
pkg_origin="chef"
pkg_version="4.0.14"
pkg_description="Supermarket is Chef's community repository for cookbooks, currently hosted at supermarket.chef.io.
Supermarket can also run internally, behind-the-firewall."
pkg_upstream_url="https://docs.chef.io/supermarket/#private-supermarket"
pkg_license=('Chef-MLSA')
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
vendor_origin="chef"
chef_automate_hab_binding_mode="relaxed"
pkg_svc_user="root"
pkg_svc_group="root"
pkg_deps=(
  chef/mlsa
  "${vendor_origin}/supermarket-sidekiq/5.1.18/20220414064110"
)

pkg_binds_optional=(
  [database]="port username password"
  [redis]="port"
)

pkg_binds=(
  [rails]="fieri-url port fqdn force-ssl"
)
# pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
# scaffolding_go_base_path=github.com/chef
# scaffolding_go_repo_name=supermarket
# scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/${pkg_name}"

do_build() {
  return 0
}

do_install() {
  # do_default_install
  # hab pkg install core/redis
  # install {{ pkg_path }}
  return 0
}

