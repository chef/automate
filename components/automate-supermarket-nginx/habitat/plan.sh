pkg_name=automate-supermarket-nginx
pkg_origin=chef
pkg_version="1.19.3.1"
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
pkg_description="NGINX web server."
pkg_license=('Apache-2.0')
pkg_upstream_url=https://nginx.org/
pkg_deps=(
  # TODO 2020-05-12: PIN PIN PIN
  #
  # All dependencies that are shared between this package and the
  # chef-server-* packages are pinned to the versions required by the
  # chef-server-* packages.
  #
  core/curl/7.68.0/20200601114640
  core/ruby27/2.7.0/20200404045319
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
)
pkg_svc_user="root"
pkg_svc_group="root"
pkg_svc_run="nginx -c $pkg_svc_config_path/nginx.conf"

pkg_binds=(
  [automate-supermarket-web]="port http-port https-port force-ssl fqdn fqdn-sanitized"
)

do_build() {
  return 0
}

do_install() {
  return 0
}
