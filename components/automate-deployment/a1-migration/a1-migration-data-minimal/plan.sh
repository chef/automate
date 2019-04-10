pkg_repo=a2
pkg_name=a1-migration-data-minimal
pkg_description="Minimal Automate 1 Migration Data -- For use with the --self-test flag. No ES2 data."
pkg_origin=devchef
pkg_version=${A1_MIGRATION_DATA_VERSION:-0.0.1}
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('UNLICENSED')
pkg_upstream_url="http://github.com/chef/automate/components/automate-deployment/a1-migration/a1-migration-data-minimal"
pkg_deps=(
  core/bash
  core/rsync
)
pkg_build_deps=(
  core/coreutils
  core/rsync
)
pkg_svc_user=root
pkg_svc_group=root

do_build() {
  return 0
}

do_strip() {
  return 0
}

do_install() {
  build_line "Copying A1 configuration and data..."

  mkdir -p ${pkg_prefix}/etc
  mkdir -p ${pkg_prefix}/var/opt/delivery/a1_pg_export
  mkdir -p ${pkg_prefix}/var/opt/delivery/elasticsearch/data
  mkdir -p ${pkg_prefix}/var/opt/delivery/compliance/profiles
  mkdir -p ${pkg_prefix}/var/opt/delivery/notifications

  rsync -chaz /src/etc/delivery/ ${pkg_prefix}/etc/delivery/
  rsync -chaz /src/var/opt/delivery/a1_pg_export/ ${pkg_prefix}/var/opt/delivery/a1_pg_export
  rsync -chaz /src/var/opt/delivery/elasticsearch/data/ ${pkg_prefix}/var/opt/delivery/elasticsearch/data
  rsync -chaz /src/var/opt/delivery/delivery/ ${pkg_prefix}/var/opt/delivery/delivery
  rsync -chaz /src/var/opt/delivery/compliance/profiles/ ${pkg_prefix}/var/opt/delivery/compliance/profiles
  rsync -chaz /src/var/opt/delivery/notifications/ ${pkg_prefix}/var/opt/delivery/notifications
  rsync -chaz /src/var/opt/delivery/delivery/git_repos/ ${pkg_prefix}/var/opt/delivery/delivery/git_repos

  if [ -d /src/etc/opscode ]; then
    mkdir -p ${pkg_prefix}/etc/opscode/
    mkdir -p ${pkg_prefix}/var/opt/opscode/upgrades
    rsync -chaz /src/etc/opscode/ ${pkg_prefix}/etc/opscode
    rsync -chaz /src/var/opt/opscode/upgrades/ ${pkg_prefix}/var/opt/opscode/upgrades
    touch ${pkg_prefix}/var/opt/opscode/bootstrapped
  fi
}
