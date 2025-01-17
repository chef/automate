#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-opensearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="1.3.20"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/1.3.20/opensearch-1.3.20-linux-x64.tar.gz"
pkg_shasum=a786fe52b4d25db85cc49f34df6118f19c434b25935f28bd98c0f874ae77ccc3


pkg_build_deps=(
  core/patchelf
)
pkg_deps=(
  core/coreutils/8.32/20240105213308
  core/glibc/2.35/20240105171810
  core/zlib/1.3/20240105173710
  core/bash/5.1/20240105214248 # hooks
  chef/mlsa/1.0.1/20240125084021
  core/curl/8.7.1/20240614090648 #healthcheck
  chef/automate-openjdk
  chef/automate-platform-tools/0.1.0/20241212061203
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

do_install() {
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

