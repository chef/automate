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
  download_file "https://github.com/go-swagger/go-swagger/releases/download/v0.19.0/swagger_linux_amd64" \
    "$CACHE_PATH/swagger" \
    "9a5dd86578a93d0e829f3607e12b8e6567fd0b5dc9ad805e1097978f30e060e2"
  chmod +x "$CACHE_PATH/swagger"
  export PATH="$PATH:$CACHE_PATH"

  make build
}

do_install() {
  mv "$PLAN_CONTEXT/../public" "$pkg_prefix/public"
}
