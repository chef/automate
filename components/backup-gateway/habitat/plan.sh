#stable channel

pkg_name=backup-gateway
pkg_version="0.1.0"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_build_deps=(core/go19 core/git core/gcc)
pkg_bin_dirs=(bin)

pkg_deps=(
  core/cacerts
  chef/automate-platform-tools
  core/curl
)


pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)

do_before() {
 GOPATH=$HAB_CACHE_SRC_PATH/$pkg_dirname
 export GOPATH
}

do_unpack() {
 git clone "https://github.com/chef/minio" "$GOPATH/src/github.com/chef/minio"
 ( cd "$GOPATH/src/github.com/chef/minio" || exit
   git checkout automate_minio
 )
}


do_build(){
  build_line "build minio"
  cd "$GOPATH/src/github.com/chef/minio"
  go build --ldflags "${GO_LDFLAGS}" -o "$pkg_prefix/bin/"
}

do_install() {
  return 0
}

do_strip() {
    return 0
}




