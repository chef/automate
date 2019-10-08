pkg_name=automate-builder-memcached
pkg_version="0.1.0"
pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_deps=(core/memcached)
pkg_exports=(
  [port]=service.port
)
pkg_exposes=(port)

do_unpack() {
    return 0
}
do_build(){
    return 0
}
do_install() {
    return 0
}
do_strip() {
    return 0
}
