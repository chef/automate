#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154

pkg_name="automate-es-gateway"
pkg_description="Proxy that provides normalized access to internal or external elasticsearch database for a2"
pkg_origin="chef"
pkg_version="0.1.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"

# TODO FIXME ??? (dan): can we do this?
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
  chef/mlsa
  core/bash
  core/curl # health_check
  core/nginx/1.17.3
  chef/automate-platform-tools
)

pkg_exports=(
  [http-host]=service.host
  [http-port]=service.port
)

pkg_binds=(
  [automate-elasticsearch]="http-port"
)

pkg_exposes=(http-port)

do_download() {
    return 0
}

do_build() {
    return 0
}

do_install() {
    return 0
}
