# shellcheck disable=SC2148
pkg_name="automate-backend-elasticsidecar"
pkg_description="Password manager for elasticsearch-odfe"
pkg_origin="chef"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_version="0.1.0"
pkg_upstream_url="http://github.com/chef/a2-ha-backend/components/automate-backend-elasticsidecar"
pkg_deps=(
  core/bash
  core/ruby26
  chef/elasticsearch-odfe
)
pkg_build_deps=(
  core/make
  core/gcc
)
pkg_interpreters=(bin/ruby)


# currently broken, these should be required binds, but they are never found
pkg_binds=(
  [elasticsearch]="http-port transport-port root-ca admin-pem admin-key admin_username admin_password dashboard_username dashboard_password"
)

pkg_lib_dirs=(lib)

do_before() {
  update_pkg_version
}

do_build() {
  return 0
}

do_install() {
  pushd habitat/automate-backend-elasticsidecar/
    bundle config set --local path "${pkg_prefix}/lib/gems"
    bundle install
  popd
  mkdir "${pkg_prefix}/bin"
  install "$PLAN_CONTEXT/bin/elastic_sidecar.rb" "${pkg_prefix}/bin/elastic_sidecar.rb"
  mkdir "${pkg_prefix}/data"
  install "$PLAN_CONTEXT/data/dashboards.tar.gz" "${pkg_prefix}/data/dashboards.tar.gz"
}

do_end() {
  return 0
}
