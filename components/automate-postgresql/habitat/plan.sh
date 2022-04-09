#stable channel

pkg_name="automate-postgresql"
pkg_description="Wrapper package for core/postgresql"
pkg_origin="chef"
pkg_version="13.5.0"
vendor_origin="core"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"

# pinned PG version. We need to also update components/automate-cli/cmd/chef-automate/migration_pg.go (line)
pg_version="13.5"

# pinned PG version. We need to also update components/automate-cli/cmd/chef-automate/migration_pg.go (line)
ident="20220120092917"
pkg_deps=(
  core/coreutils
  chef/mlsa
  ${vendor_origin}/postgresql13/${pg_version}/${ident}
)

pkg_exports=(
  [port]=service.port
  [superuser_name]=superuser.name
)

pkg_exposes=(port)

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  return 0
}
