# This is the version that the current ODFE package depends on
OPENSEARCH_VERSION="1.2.3"
#OPENSEARCH_PKG_URL="https://artifacts.opensearch.org/releases/bundle/opensearch/${OPENSEARCH_VERSION}/opensearch-${OPENSEARCH_VERSION}-linux-x64.tar.gz"
OPENSEARCH_PKG_URL="/src/opensearch-1.2.3-linux-x64.tar.gz"
pkg_version=1.2.3
pkg_name="automate-opensearch"
pkg_description="automate opensearch"
pkg_origin="chef"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Apache-2.0")
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(
  core/maven
  core/coreutils
  core/git
  core/openjdk11
  core/openssl
  core/zip
)
pkg_deps=( 
  core/bash
  core/coreutils
  core/curl
  core/glibc
  core/openjdk11
  core/procps-ng
  core/wget
  core/zlib
)

pkg_bin_dirs=(bin)
pkg_binds_optional=(
  [elasticsearch]="http-port transport-port"
)
pkg_lib_dirs=(lib)
pkg_exports=(
  [http-port]=network.port
  [transport-port]=transport.port
)
pkg_exposes=(http-port transport-port)

do_download() {
  # wget -O "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}.tar.gz" "${OPENSEARCH_PKG_URL}"
  cp "${OPENSEARCH_PKG_URL}" "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}.tar.gz"
  rm -rf ${HAB_CACHE_SRC_PATH}/security
  git clone https://github.com/opensearch-project/security.git "${HAB_CACHE_SRC_PATH}/security"
}

do_unpack() {
  tar -xzf "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}.tar.gz" -C "${HAB_CACHE_SRC_PATH}/"
}

do_build() {
  JAVA_HOME="$(pkg_path_for core/openjdk11)"
  export JAVA_HOME
}

do_install() {
  #rm -f "${pkg_prefix}/config/opensearch.yml"
  #install -vDm644 "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}/README.textile" "${pkg_prefix}/README.textile"
  install -vDm644 "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}/LICENSE.txt" "${pkg_prefix}/LICENSE.txt"
  install -vDm644 "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}/NOTICE.txt" "${pkg_prefix}/NOTICE.txt"

  cp -a "${HAB_CACHE_SRC_PATH}/opensearch-${OPENSEARCH_VERSION}/"* "${pkg_prefix}/"


  fix_interpreter "${pkg_prefix}/bin/*" core/bash bin/bash
    # intalling plugin for aws-s3
    ### echo "y" | opensearch-plugin install repository-s3
    # installing plugin for google cloud storage
    ### echo "y" | opensearch-plugin install repository-gcs
    # mkdir -p {{pkg.svc_config_path}}
    # cp "${pkg_svc_config_path}/root-ca.pem" "${pkg_prefix}/config/"
    # cp "${pkg_svc_config_path}/esnode-key.pem" "${pkg_prefix}/config/"
    # cp "${pkg_svc_config_path}/esnode.pem" "${pkg_prefix}/config/"
    #chmod 777 -R "${pkg_prefix}/"
    chown -RL root:hab "${pkg_prefix}/"
    mkdir "${pkg_prefix}/config/certificates"
    $(pkg_path_for core/bash)/bin/bash $PLAN_CONTEXT/cert-1.sh "${pkg_prefix}/config/"
    ls -ltrh
    #cp "habitat/config/opensearch.yml" "${pkg_prefix}/config/"
    # su hab -c "sh ${pkg_prefix}/bin/opensearch"
    #chmod 755 "${pkg_prefix}/securityadmin_demo.sh"
    #su hab -c "sh ${pkg_prefix}/securityadmin_demo.sh"
}