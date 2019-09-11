pkg_name=automate-chef-io
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_description="HTML for automate.chef.io"
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(
  core/git
  core/hugo
  core/jq-static
  core/make
)

do_build() {
  download_file "https://github.com/go-swagger/go-swagger/releases/download/v0.20.1/swagger_linux_amd64" \
    "$CACHE_PATH/swagger" \
    "c69da2ff7a58ee05b4fc63dbb29764cb027a97e0eaae316e1d56f586188d92b3"
  chmod +x "$CACHE_PATH/swagger"
  export PATH="$PATH:$CACHE_PATH"

  make build
}

do_install() {
  mv "$PLAN_CONTEXT/../public" "$pkg_prefix/public"
}
