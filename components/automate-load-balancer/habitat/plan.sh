pkg_name=automate-load-balancer
pkg_description="internal and external load balancer and reverse proxy for Automate 2.0"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  core/openssl
  core/nginx
  chef/mlsa
  core/bash
)

pkg_svc_user="root" # a-l-b needs to listen on 80 and 443
pkg_svc_group="hab" # ensures that hab user has access to all service dirs

pkg_exports=(
  [http-port]=http.port
  [https-port]=https.port
)

chef_automate_hab_binding_mode="relaxed"

pkg_binds_optional=(
  [automate-dex]="port"
  [automate-gateway]="port"
  [automate-ui]="port"
  [session-service]="port"
  [automate-cs-nginx]="port"
  [automate-workflow-nginx]="port"
  [automate-builder-api-proxy]="port"
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
