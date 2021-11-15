#shellcheck disable=SC2034
#shellcheck disable=SC2154

# Instead of wrapping the core/openjdk11 package, we're using our own so that
# we don't need to depend on ALSA or FreeType, which have non-compatible licenses.

pkg_origin=chef
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_name=automate-openjdk
# Release archive https://jdk.java.net/archive/
pkg_version=11.0.12+7
pkg_source=https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.12%2B7/OpenJDK11U-jdk_x64_linux_hotspot_11.0.12_7.tar.gz
pkg_shasum=8770f600fc3b89bf331213c7aa21f8eedd9ca5d96036d1cd48cb2748a3dbefd2
pkg_filename=OpenJDK11U-jdk_x64_linux_hotspot_11.0.12_7.tar.gz
pkg_dirname="jdk-${pkg_version}"
pkg_license=("GPL-2.0-with-classpath-exception")
pkg_description=('AdoptOpenJDK binaries are created from the unmodified source code at OpenJDK.')
pkg_upstream_url=https://adoptopenjdk.net/
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
    -exec sh -c 'file -i "$1" | grep -q "x-pie-executable; charset=binary"' _ {} \; \
    -exec patchelf --interpreter "$(pkg_path_for glibc)/lib/ld-linux-x86-64.so.2" --set-rpath "${LD_RUN_PATH}" {} \;

  find "$pkg_prefix/lib" -type f -name "*.so" \
    -exec patchelf --set-rpath "${LD_RUN_PATH}" {} \;

  popd || return 1
}

do_strip() {
  return 0
}
