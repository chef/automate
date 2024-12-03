#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-opensearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="2.18.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/2.18.0/opensearch-2.18.0-linux-x64.tar.gz"
pkg_shasum=ae3cb4107b2e0cdbb9b98bb4e5f2f019b736b68e995442c718e459d39ff01df1
# pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/2.11.1/opensearch-2.11.1-linux-x64.tar.gz"
# pkg_shasum=926eb3f99f592e7518d5dc1e250ae11d1bb45ef766a90222a8df11bb2ee35a9c


pkg_build_deps=(
  core/patchelf
)
pkg_deps=(
  core/coreutils
  core/glibc
  core/zlib
  core/bash # hooks
  chef/mlsa
  core/curl # health_check
  chef/automate-openjdk
  chef/automate-platform-tools
)
pkg_bin_dirs=(os/bin)
pkg_lib_dirs=(lib)

pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
  [transport-port]=transport.port
  [deprecated_external_os]=deprecated.external_os
  [deprecated_backup_location]=path.repo
  [disable]=disable
)

pkg_binds=(
  [backup-gateway]="port"
)

pkg_exposes=(http-port transport-port)

do_download() {
  do_default_download
}

do_build() {
  return 0
}

do_after() {
  if [ ! -f /usr/bin/env ]; then
    echo "inside the /usr/ib/env"
    cp $(pkg_path_for core/coreutils)/bin/env /usr/bin/env
  fi

}

do_install() {
  if [ ! -f /usr/bin/env ]; then
    echo "inside the /isr/ib/env"
    cp $(pkg_path_for core/coreutils)/bin/env /usr/bin/env
  fi

  cd "$HAB_CACHE_SRC_PATH/opensearch-${pkg_version}"
  chown -RL hab:hab ${pkg_prefix}
  mkdir -p "${pkg_prefix}/os"
  ls -ltrh
  cp -ra ./* "${pkg_prefix}/os"
  # jvm.options needs to live relative to the binary.
  # mkdir -p "$pkg_prefix/es/config"
  # install -vDm644 config/jvm.options "$pkg_prefix/es/config/jvm.options"

  # Delete unused binaries to save space
  #rm "${pkg_prefix}/os/bin/"*.bat "${pkg_prefix}/os/bin/"*.exe

  #LD_RUN_PATH=$LD_RUN_PATH:${pkg_prefix}/os/modules/x-pack-ml/platform/linux-x86_64/lib
  #export LD_RUN_PATH
  #sudo ./bin/opensearch-plugin install repository-s3
  rm -rf "${pkg_prefix}/os/jdk"

  "${pkg_prefix}/os/bin/opensearch-plugin" install -b repository-s3
  chown -RL hab:hab ${pkg_prefix}
  chown -RL hab:hab ${pkg_prefix}/*
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/securityadmin.sh"
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/install_demo_configuration.sh"
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/audit_config_migrater.sh"
}

do_strip() {
  return 0
}

