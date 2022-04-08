#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-es-gateway"
pkg_description="Proxy that provides normalized access to internal or external elasticsearch database for a2"
pkg_origin="chef"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"


jdomain_version="master"
jdomain_filename="ngx_upstream_jdomain-${jdomain_version}.tar.gz"
jdomain_source="https://github.com/wdaike/ngx_upstream_jdomain/archive/${jdomain_version}.tar.gz"
jdomain_shasum=3e7bedcddedf26d82da214d299e4cbee7605ac85a04ff3415c6b85de4f5a4ed5

nginx_version="1.21.3"
pkg_source="https://nginx.org/download/nginx-${nginx_version}.tar.gz"
pkg_dirname="nginx-${nginx_version}"
pkg_upstream_url="https://www.chef.io/automate"
pkg_shasum=14774aae0d151da350417efc4afda5cce5035056e71894836797e1f6e2d1175a


chef_automate_hab_binding_mode="relaxed"

# TODO(ssd) 2018-07-27: This instance of nginx should not have to run
# as root. This is here so that we can reliably send access logs to
# stdout. Nginx does not natively have an option to send access logs
# to stdout. To get around this, we use /dev/stdout. Unfortunately,
# because of how we are started by hab, both /dev/stdout and
# /proc/self/fd/1 both eventually point to files that are owned by
# root. We could potentially get around this with some named pipes and
# `tail -f`, but since the nginx worker threads will drop priv's to
# the `hab` user I've opted to set our svc_user as root, in line with
# the other nginx instances in A2 at the moment:
pkg_svc_user="root"

pkg_deps=(
  core/glibc
  core/libedit
  core/ncurses
  core/zlib
  core/bzip2
  core/openssl
  core/pcre

  core/coreutils
  chef/mlsa
  core/bash
  core/curl # health_check
  chef/automate-platform-tools
)

pkg_build_deps=(
  core/gcc
  core/make
  core/coreutils
)

pkg_lib_dirs=(lib)
pkg_bin_dirs=(sbin)
pkg_include_dirs=(include)


pkg_exports=(
  [http-port]=service.port
)

pkg_binds=(
  [automate-elasticsearch]="http-port"
)

pkg_exposes=(http-port)

do_download() {
  do_default_download
  pushd "${HAB_CACHE_SRC_PATH}" || return 1
  download_file "${jdomain_source}" "${jdomain_filename}" "${jdomain_shasum}"

  tar zxvf ${jdomain_filename}
  popd || return 1
}

do_build() {
  ./configure \
    --prefix="${pkg_prefix}" \
    --conf-path="${pkg_svc_config_path}/nginx.conf" \
    --sbin-path="${pkg_prefix}/bin/nginx" \
    --pid-path="${pkg_svc_var_path}/nginx.pid" \
    --lock-path="${pkg_svc_var_path}/nginx.lock" \
    --user=hab \
    --group=hab \
    --http-log-path=/dev/stdout \
    --error-log-path=stderr \
    --http-client-body-temp-path="${pkg_svc_var_path}/client-body" \
    --http-proxy-temp-path="${pkg_svc_var_path}/proxy" \
    --http-fastcgi-temp-path="${pkg_svc_var_path}/fastcgi" \
    --http-scgi-temp-path="${pkg_svc_var_path}/scgi" \
    --http-uwsgi-temp-path="${pkg_svc_var_path}/uwsgi" \
    --with-pcre \
    --with-pcre-jit \
    --with-file-aio \
    --with-stream=dynamic \
    --with-stream_ssl_module \
    --with-mail=dynamic \
    --with-http_gunzip_module \
    --with-http_gzip_static_module \
    --with-http_realip_module \
    --with-http_v2_module \
    --with-http_ssl_module \
    --with-http_stub_status_module \
    --with-http_addition_module \
    --with-http_degradation_module \
    --with-http_flv_module \
    --with-http_mp4_module \
    --with-http_secure_link_module \
    --with-http_sub_module \
    --with-http_slice_module \
    --with-cc-opt="${CFLAGS}" \
    --with-ld-opt="${LDFLAGS}" \
    --add-module=${HAB_CACHE_SRC_PATH}/ngx_upstream_jdomain-${jdomain_version}

  make
}

do_install() {
  make install
  mkdir -p "${pkg_prefix}/sbin"
  cp "${HAB_CACHE_SRC_PATH}/${pkg_dirname}/objs/nginx" "${pkg_prefix}/sbin"
}

