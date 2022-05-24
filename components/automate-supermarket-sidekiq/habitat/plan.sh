pkg_name=automate-supermarket-sidekiq
pkg_origin="chef"
pkg_version="4.0.14"
pkg_description="Supermarket is Chef's community repository for cookbooks, currently hosted at supermarket.chef.io.
Supermarket can also run internally, behind-the-firewall."
pkg_upstream_url="https://docs.chef.io/supermarket/#private-supermarket"
pkg_license=('Chef-MLSA')
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
vendor_origin="chef"
chef_automate_hab_binding_mode="relaxed"
pkg_svc_user="root"
pkg_svc_group="root"
pkg_deps=(
  core/ruby27/2.7.4/20211019113603
  core/node/14.16.1/20211016165647
  core/libxml2/2.9.10/20210826132717
  core/libxslt/1.1.34/20210826231334
  core/postgresql-client/9.6.24/20220217081107
  core/busybox-static/1.33.0/20210826062032
  core/coreutils/8.32/20210826054709
  core/bash/5.1/20210826055113
  core/file/5.39/20210826050928
  core/glibc/2.33/20210826050111
  core/gcc-libs/9.3.0/20210826054136
  core/libarchive/3.5.1/20211016174003
  core/shared-mime-info/1.10/20211019161119
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  "${vendor_origin}/supermarket-sidekiq/5.1.18/20220414064110"
)

chef_automate_hab_binding_mode="relaxed"
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

pkg_binds_optional=(
  [automate-pg-gateway]="port username password"
  [automate-supermarket-redis]="port"
)

pkg_binds=(
  [automate-supermarket]="fieri-url port fqdn force-ssl"
)

do_build() {
  return 0
}

do_install() {
  return 0
}

