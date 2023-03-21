#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#stable channel

pkg_name="automate-cs-ocid"
pkg_description="Wrapper package for chef/ocid"
pkg_origin="chef"
# WARNING: Version managed by .expeditor/update_chef_server.sh
pkg_version="15.4.0"
vendor_origin="chef"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_upstream_url="https://www.chef.io/automate"
pkg_deps=(
  core/git
  core/make
  core/gcc
  core/tar
  core/pkg-config
  core/coreutils
  core/libxml2
  core/libxslt
  chef/mlsa
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/oc_id/15.4.0/20230105061030"
)

pkg_binds=(
  [automate-pg-gateway]="port"
  [pg-sidecar-service]="port"
)
pkg_exports=(
  [http-host]=network.host
  [http-port]=network.port
)

pkg_exposes=(http-port)

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

do_download() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  export HOME="${pkg_prefix}/oc_id"
  mkdir $HOME

  export GEM_HOME="${pkg_prefix}/vendor/bundle"
  mkdir -p "$GEM_HOME"

  { git ls-files; git ls-files --exclude-standard --others; } \
      | _tar_pipe_app_cp_to "$HOME"
  bundle config path ${HOME}/vendor/bundle
  bundle config build.sqlite3 --with-sqlite3-lib=$(pkg_path_for core/sqlite)/lib
  bundle config build.nokogiri --with-xml2-include=$(pkg_path_for core/libxml2)/include/libxml2 \
    --with-xml2-lib=$(pkg_path_for core/libxml2)/lib \
    --with-xslt-include=$(pkg_path_for core/libxslt)/include/libxslt \
    --with-xslt-lib=$(pkg_path_for core/libxslt)/lib
  bundle install --path "${HOME}/vendor/bundle" --binstubs="${HOME}/bin" --shebang ruby --deployment
  # fix tzdata location
  echo "Adding core/tzdata zoneinfo search path to tzinfo gem"
  grep -l DEFAULT_SEARCH_PATH $HOME/vendor/bundle/ruby/*/gems/tzinfo*/lib/tzinfo/zoneinfo_data_source.rb | while read -r f; do
    sed -e "s,/etc/zoneinfo,$(pkg_path_for core/tzdata)/share/zoneinfo,g" -i "$f"
  done
}


_tar_pipe_app_cp_to() {
  local dst_path tar
  dst_path="$1"
  tar="$(pkg_path_for tar)/bin/tar"

  "$tar" -cp \
      --no-xattrs \
      --exclude-backups \
      --exclude-vcs \
      --exclude='habitat' \
      --exclude='vendor/bundle' \
      --exclude='results' \
      --files-from=- \
      -f - \
  | "$tar" -x \
      --no-same-owner \
      -C "$dst_path" \
      -f -
}

