pkg_name="automate-elasticsearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="6.2.2"
vendor_origin="core"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(
  ${vendor_origin}/elasticsearch/${pkg_version}
)
pkg_deps=(
  chef/mlsa
  core/glibc
  core/coreutils-static
  core/curl # health_check
  core/unzip
  core/grep
  core/jre8
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
    download_file "https://artifacts.elastic.co/downloads/elasticsearch-plugins/repository-s3/repository-s3-${pkg_version}.zip" "repository-s3.zip" "70a7ed6ad1597d91dcb20ec7cd3da3701ea66f70347d28d133649998778e0b8d"
}

do_build() {
    return 0
}

do_install() {
    cp -a "$(pkg_path_for ${vendor_origin}/elasticsearch)/es/"* "${pkg_prefix}/es/"
    "${pkg_prefix}/es/bin/elasticsearch-plugin" install -b "file://${HAB_CACHE_SRC_PATH}/repository-s3.zip"
}
