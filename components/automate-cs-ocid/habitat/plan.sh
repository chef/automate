#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-cs-ocid"
pkg_description="Wrapper package for chef/ocid"
pkg_origin="chef"
# WARNING: Version managed by .expeditor/update_chef_server.sh
pkg_version="15.8.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")

pkg_svc_user=root
pkg_svc_group=root
pkg_svc_run="return 0"

pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/oc_id/15.8.0/20230929110850"
)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
)

pkg_exposes=(http-port)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

# do_prepare() {
#   GO_LDFLAGS="-X main.RubyPath=$(hab pkg path 'core/ruby30')"
#   export GO_LDFLAGS

#   build_line "Setting link for /usr/bin/env to 'coreutils'"
#   [[ ! -f /usr/bin/env ]] && ln -s "$(pkg_path_for coreutils)/bin/env" /usr/bin/env
#   return 0
# }

do_download() {
  return 0
}

do_build() {
   # TODO :: Remove following section once new ocid hab package is used which includes gem: "tzinfo-data"
  # cd $(pkg_path_for "chef/oc_id")/oc_id
  # bundle config path "vendor/bundle"
  # if ! grep -q "gem 'tzinfo-data'" Gemfile; then
  #   echo "gem 'tzinfo-data'" >> Gemfile
  #   bundle install
  # fi

  return 0
}

do_install() {
  return 0
}
