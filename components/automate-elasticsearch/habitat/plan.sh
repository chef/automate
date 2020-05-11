#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name="automate-elasticsearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="6.8.3"
vendor_origin="core"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(
  "${vendor_origin}/elasticsearch/${pkg_version}"
)
pkg_deps=(
  chef/mlsa
  core/glibc
  core/coreutils-static
  core/curl # health_check
  core/unzip
  core/grep
  chef/automate-openjdk
  chef/automate-platform-tools
)
pkg_bin_dirs=(es/bin)
pkg_lib_dirs=(lib)

pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
  [transport-port]=transport.port
  [deprecated_external_es]=deprecated.external_es
  [deprecated_backup_location]=path.repo
  [disable]=disable
)

pkg_binds=(
  [backup-gateway]="port"
)

pkg_exposes=(http-port transport-port)

do_download() {
  download_file "https://artifacts.elastic.co/downloads/elasticsearch-plugins/repository-s3/repository-s3-${pkg_version}.zip" "repository-s3.zip" "3dc05d6c20e683596ddabfcc3f63c9d4e9680da75bff1c904566b5508584a6d6"
  download_file "https://artifacts.elastic.co/downloads/elasticsearch-plugins/repository-gcs/repository-gcs-${pkg_version}.zip" "repository-gcs.zip" "73d9addc079609eb262a6e0415e0899f19b3867aeb13734799795e68f7a553e8"
}

do_build() {
  return 0
}

do_install() {
  cp -a "$(pkg_path_for ${vendor_origin}/elasticsearch)/es/"* "${pkg_prefix}/es/"
  "${pkg_prefix}/es/bin/elasticsearch-plugin" install -b "file://${HAB_CACHE_SRC_PATH}/repository-s3.zip"
  "${pkg_prefix}/es/bin/elasticsearch-plugin" install -b "file://${HAB_CACHE_SRC_PATH}/repository-gcs.zip"
}

do_strip() {
  return 0
}
