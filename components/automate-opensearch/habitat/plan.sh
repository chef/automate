#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-opensearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="1.2.4"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/1.2.4/opensearch-1.2.4-linux-x64.tar.gz"
pkg_shasum=d40f2696623b6766aa235997e2847a6c661a226815d4ba173292a219754bd8a8

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
pkg_bin_dirs=(os/bin)
pkg_lib_dirs=(lib)

pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
  [transport-port]=transport.port
  [deprecated_external_os]=deprecated.external_os
  [deprecated_backup_location]=path.repo
  [disable]=disable
  []
)

pkg_binds=(
  [backup-gateway]="port"
)

pkg_exposes=(http-port transport-port)

do_download() {
  do_default_download
  #wget $pkg_source
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
  echo "what is HAB_CACHE_SRC_PATH : "$HAB_CACHE_SRC_PATH
  echo "what is pkg_prefix : "${pkg_prefix}
  echo "where i am : " 
  pwd
  # jvm.options needs to live relative to the binary.
  # mkdir -p "$pkg_prefix/es/config"
  # install -vDm644 config/jvm.options "$pkg_prefix/es/config/jvm.options"

  # Delete unused binaries to save space
  #rm "${pkg_prefix}/os/bin/"*.bat "${pkg_prefix}/os/bin/"*.exe

  #LD_RUN_PATH=$LD_RUN_PATH:${pkg_prefix}/os/modules/x-pack-ml/platform/linux-x86_64/lib
  #export LD_RUN_PATH
  #sudo ./bin/opensearch-plugin install repository-s3
  "${pkg_prefix}/os/bin/opensearch-plugin" install -b repository-s3
  chown -RL hab:hab ${pkg_prefix}
  chown -RL hab:hab ${pkg_prefix}/*
<<<<<<< HEAD
  chmod 777 "{{pkg_prefix}}/os/plugins/opensearch-security/tools/securityadmin.sh"
  chmod 777 "{{pkg_prefix}}/os/plugins/opensearch-security/tools/install_demo_configuration.sh"
  chmod 777 "{{pkg_prefix}}/os/plugins/opensearch-security/tools/audit_config_migrater.sh"
=======
  chmod 777 "${pkg_prefix}/os/plugins/opensearch-security/tools/securityadmin.sh"
  chmod 777 "${pkg_prefix}/os/plugins/opensearch-security/tools/install_demo_configuration.sh"
  chmod 777 "${pkg_prefix}/os/plugins/opensearch-security/tools/audit_config_migrater.sh"
>>>>>>> 70fae7cb172ad129443669001e394f12a52df735
}

do_strip() {
  return 0
}
