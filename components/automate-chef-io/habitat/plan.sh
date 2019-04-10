pkg_name=automate-chef-io
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_description="HTML for automate.chef.io"
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(
  core/hugo
  core/make
  core/git
)

do_build() {
  make themes/chef
  $(pkg_path_for core/hugo)/bin/hugo
}

do_install() {
  mv "$PLAN_CONTEXT/../public" "$pkg_prefix/public"
}
