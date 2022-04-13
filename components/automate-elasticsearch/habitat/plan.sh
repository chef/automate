#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-elasticsearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="6.8.23"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-${pkg_version}.tar.gz"
pkg_shasum=424af91f838f9e5f13e0292f97cbd6333535450291a621d761bd479dfc2dff78

pkg_build_deps=(
  core/patchelf
)
pkg_deps=(
  core/coreutils
  core/glibc
  core/zlib

  chef/mlsa
  core/curl # health_check
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
  do_default_download
  download_file "https://artifacts.elastic.co/downloads/elasticsearch-plugins/repository-s3/repository-s3-${pkg_version}.zip" "repository-s3.zip" "3dc05d6c20e683596ddabfcc3f63c9d4e9680da75bff1c904566b5508584a6d6"
}

do_build() {
  return 0
}

do_install() {

  cd "$HAB_CACHE_SRC_PATH/elasticsearch-${pkg_version}"
  install -vDm644 README.textile "${pkg_prefix}/README.textile"
  install -vDm644 LICENSE.txt "${pkg_prefix}/LICENSE.txt"
  install -vDm644 NOTICE.txt "${pkg_prefix}/NOTICE.txt"

  # Elasticsearch is greedy when grabbing config files from /bin/..
  # so we need to put the untemplated config dir out of reach
  mkdir -p "${pkg_prefix}/es"
  cp -a ./* "${pkg_prefix}/es"

  # jvm.options needs to live relative to the binary.
  # mkdir -p "$pkg_prefix/es/config"
  # install -vDm644 config/jvm.options "$pkg_prefix/es/config/jvm.options"

  # Delete unused binaries to save space
  rm "${pkg_prefix}/es/bin/"*.bat "${pkg_prefix}/es/bin/"*.exe

  LD_RUN_PATH=$LD_RUN_PATH:${pkg_prefix}/es/modules/x-pack-ml/platform/linux-x86_64/lib
  export LD_RUN_PATH

  for bin in autoconfig autodetect categorize controller normalize; do
    build_line "patch ${pkg_prefix}/es/modules/x-pack-ml/platform/linux-x86_64/bin/${bin}"
    patchelf --interpreter "$(pkg_path_for glibc)/lib/ld-linux-x86-64.so.2" --set-rpath "${LD_RUN_PATH}" \
      "${pkg_prefix}/es/modules/x-pack-ml/platform/linux-x86_64/bin/${bin}"
  done

  find "${pkg_prefix}/es/modules/x-pack-ml/platform/linux-x86_64/lib" -type f -name "*.so" \
      -exec patchelf --set-rpath "${LD_RUN_PATH}" {} \;

  "${pkg_prefix}/es/bin/elasticsearch-plugin" install -b "file://${HAB_CACHE_SRC_PATH}/repository-s3.zip"
}

do_strip() {
  return 0
}

