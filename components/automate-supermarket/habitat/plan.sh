pkg_name="automate-supermarket"
pkg_origin="chef"
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
pkg_license=("Apache-2.0")
pkg_scaffolding="core/scaffolding-ruby"
pkg_description="Supermarket is Chef's community repository for cookbooks, currently hosted at supermarket.chef.io.
Supermarket can also run internally, behind-the-firewall."
pkg_upstream_url="https://docs.chef.io/supermarket/#private-supermarket"
pkg_build_deps=(core/phantomjs core/yarn)
pkg_svc_user="root"
pkg_svc_group="root"
pkg_version="1.19.3.1"
vendor_origin="chef"

pkg_binds_optional=(
  [automate-postgresql]="port username"
  [automate-supermarket-redis]="port"
)

pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/supermarket/5.1.5/20220322060835"
)

pkg_exports=(
  [port]=app.port
  [http-port]=nginx.port
  [https-port]=nginx.ssl_port
  [force-ssl]=nginx.force_ssl
  [fqdn]=app.fqdn
  [fqdn-sanitized]=app.fqdn_sanitized
  [fieri-url]=fieri.url
)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
