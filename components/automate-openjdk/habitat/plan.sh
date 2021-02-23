#shellcheck disable=SC2034
#shellcheck disable=SC2154

# Instead of wrapping the core/openjdk11 package, we're using our own so that
# we don't need to depend on ALSA or FreeType, which have non-compatible licenses.

pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_name=automate-openjdk
# Release archive https://jdk.java.net/archive/
pkg_version=15.0.2
pkg_source=https://download.java.net/java/GA/jdk15.0.2/0d1cfde4252546c6931946de8db48ee2/7/GPL/openjdk-${pkg_version}_linux-x64_bin.tar.gz
pkg_shasum=91ac6fc353b6bf39d995572b700e37a20e119a87034eeb939a6f24356fbcd207
pkg_filename=openjdk-${pkg_version}_linux-x64_bin.tar.gz
pkg_dirname="jdk-${pkg_version}"
pkg_license=("GPL-2.0-with-classpath-exception")
pkg_description=('The official Reference Implementation for Java SE 15 Platform')
pkg_upstream_url=https://openjdk.java.net/
pkg_deps=(
  core/gcc-libs
  core/glibc
  core/libxext
  core/libxi
  core/libxrender
  core/libxtst
  core/xlib
  core/zlib
)
pkg_build_deps=(
  core/patchelf
  core/rsync
)
pkg_bin_dirs=(bin)
pkg_lib_dirs=(lib)
pkg_include_dirs=(include)

source_dir=${HAB_CACHE_SRC_PATH}/${pkg_dirname}

do_setup_environment() {
 set_runtime_env JAVA_HOME "${pkg_prefix}"
}

do_build() {
  return 0
}

do_install() {
  pushd "${pkg_prefix}" || return 1
  rsync -avz "${source_dir}/" .

  export LD_RUN_PATH="${LD_RUN_PATH}:${pkg_prefix}/lib/jli:${pkg_prefix}/lib/server:${pkg_prefix}/lib"

  build_line "Setting interpreter for all executables to '$(pkg_path_for glibc)/lib/ld-linux-x86-64.so.2'"
  build_line "Setting rpath for all libraries to '$LD_RUN_PATH'"

  find "$pkg_prefix"/{lib,bin} -type f -executable \
    -exec sh -c 'file -i "$1" | grep -q "x-executable; charset=binary"' _ {} \; \
    -exec patchelf --interpreter "$(pkg_path_for glibc)/lib/ld-linux-x86-64.so.2" --set-rpath "${LD_RUN_PATH}" {} \;

  find "$pkg_prefix/lib" -type f -name "*.so" \
    -exec patchelf --set-rpath "${LD_RUN_PATH}" {} \;

  popd || return 1
}

do_strip() {
  return 0
}
