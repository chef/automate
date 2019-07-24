pkg_name=automate-dex
pkg_description="Automate specific wrapper plan for Dex"
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="https://www.chef.io/automate"
pkg_exports=(
  [port]=service.port
  [host]=service.host
  [grpc-host]=grpc.host
  [grpc-port]=grpc.port
)
pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_deps=(
  core/dex/2.17.0
  chef/mlsa
  ${local_platform_tools_origin:-chef}/automate-platform-tools
  core/bash
  core/curl # health_check hook
)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"

do_build() {
  return 0
}

do_install() {
  build_line "copying chef theme content"
  # Copy static assets from core/dex
  mkdir -p "${pkg_prefix}/web"
  cp -r "$(pkg_path_for core/dex)/web/static" "${pkg_prefix}/web/"
  cp -r "$(pkg_path_for core/dex)/web/templates" "${pkg_prefix}/web/"
  # Copy our custom theme into place
  mkdir -p "${pkg_prefix}/web/themes/chef"
  cp -r "$PLAN_CONTEXT/../web/theme/"* "${pkg_prefix}/web/themes/chef"
  cp -r "$PLAN_CONTEXT/../web/templates/"* "${pkg_prefix}/web/templates"
  cp -r "$PLAN_CONTEXT/../web/static"/* "${pkg_prefix}/web/static"
}
