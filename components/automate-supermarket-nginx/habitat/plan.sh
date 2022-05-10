pkg_name=automate-supermarket-nginx
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_version="1.19.3"
pkg_description="NGINX web server."
pkg_license=('Apache-2.0')
pkg_upstream_url=https://nginx.org/
vendor_origin="chef"
pkg_deps=( 
  core/coreutils
  chef/mlsa
  # TODO 2020-05-12: PIN PIN PIN
  #
  # All dependencies that are shared between this package and the
  # chef-server-* packages are pinned to the versions required by the
  # chef-server-* packages.
  #
  core/curl
  core/ruby27 
  "${vendor_origin}/supermarket-nginx/1.19.3.1/20220321050811"
)
pkg_svc_user="root"
pkg_svc_group="root"
pkg_svc_run="nginx -c $pkg_svc_config_path/nginx.conf"

pkg_binds=(
  [automate-supermarket]="port http-port https-port force-ssl fqdn fqdn-sanitized"
)

pkg_svc_user="root"

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"

chef_automate_hab_binding_mode="relaxed"


do_build() {
  return 0
}

do_install() {
  return 0
}