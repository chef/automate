pkg_repo=a2
pkg_name=a1-migration-data-full
pkg_description="Automate 1 Migration Data Full -- contains ES2 indexes"
pkg_origin=chef
pkg_version=${A1_MIGRATION_DATA_VERSION:-0.0.1}
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('UNLICENSED')
pkg_upstream_url="http://github.com/chef/automate/components/automate-deployment/a1-migration/a1-migration-data"
pkg_deps=(
  core/bash
  core/rsync
  core/coreutils
)
pkg_build_deps=(
  core/coreutils
  core/rsync
)
pkg_svc_user=root
pkg_svc_group=root
# Workaround to ensure install hook runs as root:
# https://github.com/habitat-sh/habitat/issues/6341
pkg_svc_run="return 0"

do_build() {
  return 0
}

do_strip() {
  return 0
}

do_install() {
  build_line "Copying A1 configuration and data..."

  mkdir -p ${pkg_prefix}/etc
  mkdir -p ${pkg_prefix}/var/opt/delivery/compliance/profiles
  mkdir -p ${pkg_prefix}/var/opt/delivery/notifications
  mkdir -p ${pkg_prefix}/var/opt/delivery/postgresql

  rsync -chaz /src/etc/delivery/ ${pkg_prefix}/etc/delivery/
  rsync -chaz /src/var/opt/delivery/delivery/ ${pkg_prefix}/var/opt/delivery/delivery
  rsync -chaz /src/var/opt/delivery/compliance/profiles/ ${pkg_prefix}/var/opt/delivery/compliance/profiles
  rsync -chaz /src/var/opt/delivery/notifications/ ${pkg_prefix}/var/opt/delivery/notifications
  rsync -chaz /src/var/opt/delivery/postgresql/ ${pkg_prefix}/var/opt/delivery/postgresql
  rsync -chaz /src/var/opt/delivery/delivery/git_repos/ ${pkg_prefix}/var/opt/delivery/delivery/git_repos

  # Load the ES 2 sample data so that our full migration includes v2 indices
  tar zxvf /src/a1-migration/a1-es2-sample-data-2018-05-09-21-34.tgz -C ${pkg_prefix}

  if [ -d /src/etc/opscode ]; then
    mkdir -p ${pkg_prefix}/etc/opscode/
    mkdir -p ${pkg_prefix}/var/opt/opscode/upgrades
    rsync -chaz /src/etc/opscode/ ${pkg_prefix}/etc/opscode
    rsync -chaz /src/var/opt/opscode/upgrades/ ${pkg_prefix}/var/opt/opscode/upgrades
    touch ${pkg_prefix}/var/opt/opscode/bootstrapped
  fi
}
