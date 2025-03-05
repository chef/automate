#shellcheck disable=SC2034
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-ha-opensearch"
pkg_description="Wrapper package for core/elasticsearch"
pkg_origin="chef"
pkg_version="1.3.20"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/1.3.20/opensearch-1.3.20-linux-x64.tar.gz"
pkg_shasum=a786fe52b4d25db85cc49f34df6118f19c434b25935f28bd98c0f874ae77ccc3
pkg_build_deps=(
  core/patchelf/0.13/20240105212025
  core/make/4.3/20240105222044
  core/gcc/9.5.0/20240105175314
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
  core/ruby3_4
)
pkg_interpreters=(bin/ruby)
pkg_bin_dirs=(os/bin)
pkg_lib_dirs=(lib)

pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
  [transport-port]=transport.port
  [deprecated_external_es]=deprecated.external_es
  [deprecated_backup_location]=path.repo
  [disable]=disable
  [root-ca]=tls.rootCA
  [admin-pem]=tls.admin_cert
  [admin-key]=tls.admin_key
  [admin_username]=opensearch_auth.admin_username
  [admin_password]=opensearch_auth.admin_password
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

  chown -RL hab:hab ${pkg_prefix}
  # Opensearch is greedy when grabbing config files from /bin/..
  # so we need to put the untemplated config dir out of reach
  mkdir -p "${pkg_prefix}/os"
  ls -ltrh "$HAB_CACHE_SRC_PATH/opensearch-${pkg_version}"
  cp -ra "$HAB_CACHE_SRC_PATH/opensearch-${pkg_version}"/* "${pkg_prefix}/os"
  echo "what is HAB_CACHE_SRC_PATH : "$HAB_CACHE_SRC_PATH
  echo "what is pkg_prefix : "${pkg_prefix}
  echo "where i am : "
  pwd

  "${pkg_prefix}/os/bin/opensearch-plugin" install -b repository-s3
  "${pkg_prefix}/os/bin/opensearch-plugin" install -b repository-gcs
  
  chown -RL hab:hab ${pkg_prefix}
  chown -RL hab:hab ${pkg_prefix}/*

  mkdir "${pkg_prefix}/os/config/certificates"
  $(pkg_path_for core/bash)/bin/bash $PLAN_CONTEXT/cert.sh "$PLAN_CONTEXT"
  chown -RL hab:hab ${pkg_prefix}/os/config/*
  chmod -R 777 ${pkg_prefix}/*

  echo "changing permission for securityadmin.sh file"
  echo "......................................................................."
  chown -RL hab:hab ${pkg_prefix}
  chown -RL hab:hab ${pkg_prefix}/*
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/securityadmin.sh"
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/install_demo_configuration.sh"
  chmod 755 "${pkg_prefix}/os/plugins/opensearch-security/tools/audit_config_migrater.sh"
  echo "......................................................................."

  echo "changing permission for roles and internal user yml file."
  chmod 775 $PLAN_CONTEXT/config/securityconfig/internal_users.yml
  chmod 775 $PLAN_CONTEXT/config/securityconfig/roles_mapping.yml
  echo "......................................................................."

  chmod 775 $(pkg_path_for chef/automate-openjdk)/lib/security/cacerts


}

do_strip() {
  return 0
}

