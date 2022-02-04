pkg_name="automate-opensearch"
pkg_description="Wrapper package for core/opensearch"
pkg_origin="chef"
pkg_version="1.2.3"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_source="https://artifacts.opensearch.org/releases/bundle/opensearch/${pkg_version}/opensearch-${pkg_version}-linux-x64.tar.gz"
#           https://artifacts.opensearch.org/releases/plugins/repository-s3/1.0.0/repository-s3-1.0.0-rc1.zip
pkg_shasum=a594ac2808e6e476d647e47e45e837fba3e21671285ce39b2cee1c32d5e6887d
#f8a933e19fd43141be8bde21422ba8a8c2b407ed
## that's how we calculate shasum
## /usr/bin/shasum ~/Desktop/opensearch/opensearch-1.2.3-linux-x64.tar.gz

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
## Open search : Red Hat Enterprise Linux 7, 8; CentOS 7, 8; Amazon Linux 2; Ubuntu 16.04, 18.04, 20.04
pkg_exposes=(http-port transport-port)


do_download() {
    do_default_download
    #wget $pkg_source
}

do_build() {
  return 0
}

do_install() {
    # download_file $pkg_source
    echo '========================================================'
    pwd
    echo ${pkg_prefix}
    ls -l ${pkg_prefix}
    #tar -zxf ${pkg_prefix}/opensearch-${pkg_version}-linux-x64.tar.gz
    #cd ${pkg_prefix}/opensearch-${pkg_version}
    echo '========================================================'
    echo 'es'
    ls -l ${pkg_prefix}/es
    ls -l ${pkg_prefix}/es/bin
    
    echo '========================================================'
    echo 'lib'
    ls -l ${pkg_prefix}/lib
    # below command will give the list of available plugin
    # bin/opensearch-plugin list
    # Need to download the plugin for aws-s3
    ${pkg_prefix}/es/bin/opensearch-plugin install repository-s3
    # need to download the plugin for google cloud storage
    ${pkg_prefix}/es/bin/opensearch-plugin install repository-gcs
}

do_strip() {
  return 0
}
