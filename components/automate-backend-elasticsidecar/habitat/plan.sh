# shellcheck disable=SC2148
# stable channel

pkg_name="automate-ha-elasticsidecar"
pkg_description="Sidecar for HA Opensearch"
pkg_origin="chef"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_version="0.1.0"
pkg_upstream_url="http://github.com/chef/a2-ha-backend/components/automate-backend-elasticsidecar"
pkg_deps=(
  core/bash
  core/ruby30
  chef/automate-openjdk
  chef/automate-ha-opensearch
)
pkg_build_deps=(
  core/make
  core/gcc
)
pkg_interpreters=(bin/ruby)

pkg_binds=(
  [opensearch]="http-port transport-port root-ca admin-pem admin-key admin_username admin_password"
)

pkg_lib_dirs=(lib)

do_before() {
  update_pkg_version
}

do_build() {
  return 0
}

do_install() {
  gem update --system
  gem install bcrypt --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install http --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install json --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install toml-rb --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install mixlib-shellout --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install pry --no-document --install-dir "${pkg_prefix}/lib/gems"
  gem install chefstyle --no-document --install-dir "${pkg_prefix}/lib/gems"
  mkdir "${pkg_prefix}/bin"
  install "$PLAN_CONTEXT/bin/elastic_sidecar.rb" "${pkg_prefix}/bin/elastic_sidecar.rb"
  mkdir "${pkg_prefix}/data"
}

do_end() {
  return 0
}

