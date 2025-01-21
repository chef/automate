#stable channel

pkg_name=automate-load-balancer
pkg_origin=chef
pkg_version="0.1.0"
pkg_description="internal and external load balancer and reverse proxy for Automate 2.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
nginx_version="1.25.4"
headers_more_version="0.36"
ngx_devel_kit_version="0.3.2"
set_misc_version="0.33"
headers_more_filename="headers-more-nginx-module-${headers_more_version}.tar.gz"
headers_more_source="https://github.com/openresty/headers-more-nginx-module/archive/v${headers_more_version}.tar.gz"
headers_more_shasum=7c1f7bb13e79433ee930c597d272a64bc6e30c356a48524f38fd34fa88d62473
ngx_devel_kit_filename="ngx_devel_kit-${ngx_devel_kit_version}.tar.gz"
ngx_devel_kit_source="https://github.com/vision5/ngx_devel_kit/archive/v${ngx_devel_kit_version}.tar.gz"
ngx_devel_kit_shasum=aa961eafb8317e0eb8da37eb6e2c9ff42267edd18b56947384e719b85188f58b
set_misc_filename="set-misc-nginx-module-${set_misc_version}.tar.gz"
set_misc_source="https://github.com/openresty/set-misc-nginx-module/archive/v${set_misc_version}.tar.gz"
set_misc_shasum=cd5e2cc834bcfa30149e7511f2b5a2183baf0b70dc091af717a89a64e44a2985
pkg_source="https://nginx.org/download/nginx-${nginx_version}.tar.gz"
pkg_dirname="nginx-${nginx_version}"
pkg_upstream_url="https://www.chef.io/automate"
pkg_shasum=760729901acbaa517996e681ee6ea259032985e37c2768beef80df3a877deed9

pkg_deps=(
  core/glibc/2.35/20240105171810
  core/libedit/20210910-3.1/20240106023704
  core/ncurses/6.2/20240105212325
  core/zlib/1.3/20240105173710
  core/bzip2/1.0.8/20240105212113
  core/openssl/1.0.2zi/20240105224424
  #core/pcre2
  core/pcre/8.45/20240105213900
  chef/mlsa/1.0.1/20240125084021
  core/bash/5.1/20240105214248
)

pkg_build_deps=(
  core/gcc/9.5.0/20240105175314
  core/make/4.3/20240105222044
  core/coreutils/8.32/20240105213308
)
pkg_lib_dirs=(lib)
pkg_bin_dirs=(sbin)
pkg_include_dirs=(include)

pkg_svc_user="root" # a-l-b needs to listen on 80 and 443
pkg_svc_group="hab" # ensures that hab user has access to all service dirs

pkg_exports=(
  [http-port]=http.port
  [https-port]=https.port
)

chef_automate_hab_binding_mode="relaxed"

pkg_binds_optional=(
  [automate-dex]="port"
  [automate-gateway]="port"
  [automate-ui]="port"
  [session-service]="port"
  [automate-cs-nginx]="port"
  [automate-builder-api-proxy]="port"
)

do_download() {
  cp -f cacert.pem /hab/pkgs/core/cacerts/2021.10.26/20240105224256/ssl/certs/cacert.pem
  do_default_download
  pushd "${HAB_CACHE_SRC_PATH}" || return 1
  download_file "${headers_more_source}" "${headers_more_filename}" "${headers_more_shasum}"
  tar zxvf ${headers_more_filename}

  download_file "${set_misc_source}" "${set_misc_filename}" "${set_misc_shasum}"
  tar zxvf ${set_misc_filename}

  download_file "${ngx_devel_kit_source}" "${ngx_devel_kit_filename}" "${ngx_devel_kit_shasum}"
  tar zxvf ${ngx_devel_kit_filename}
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
    --add-module=${HAB_CACHE_SRC_PATH}/headers-more-nginx-module-${headers_more_version} \
    --add-module=${HAB_CACHE_SRC_PATH}/ngx_devel_kit-${ngx_devel_kit_version} \
    --add-module=${HAB_CACHE_SRC_PATH}/set-misc-nginx-module-${set_misc_version}

  make
}

do_install() {
  make install
  mkdir -p "${pkg_prefix}/sbin"
  cp "${HAB_CACHE_SRC_PATH}/${pkg_dirname}/objs/nginx" "${pkg_prefix}/sbin"
}



