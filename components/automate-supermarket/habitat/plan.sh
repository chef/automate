pkg_name="automate-supermarket"
pkg_origin="chef"
pkg_maintainer="The Chef Maintainers <humans@chef.io>"
pkg_license=("Apache-2.0")
pkg_description="Supermarket is Chef's community repository for cookbooks, currently hosted at supermarket.chef.io.
Supermarket can also run internally, behind-the-firewall."
pkg_upstream_url="https://www.chef.io/automate"
pkg_build_deps=(core/phantomjs core/yarn)
pkg_svc_user="root"
pkg_svc_group="root"
pkg_version="5.1.5"
vendor_origin="chef"
scaffolding_ruby_pkg="core/ruby27"
# pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
# automate_scaffolding_include_templates=(sqerl.config)

pkg_binds=(
  [automate-pg-gateway]="port"
  [automate-supermarket-redis]="port"
)

pkg_deps=(
  core/coreutils
  core/bash 
  core/file 
  core/glibc 
  core/gcc-libs 
  core/libarchive 
  core/shared-mime-info
  core/bundler
  "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # "${local_platform_tools_origin:-chef}/automate-platform-tools"
  # WARNING: Version pin managed by .expeditor/update_chef_server.sh
  "${vendor_origin}/supermarket/5.1.5/20220322060835"
)

pkg_exports=(
  [port]=app.port
  [http-port]=nginx.port
  [https-port]=nginx.ssl_port
  [force-ssl]=nginx.force_ssl
  [fqdn]=app.fqdn
  [fqdn-sanitized]=app.fqdn_sanitized
  [fieri-url]=fieri.url
)

chef_automate_hab_binding_mode="relaxed"

pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding"
automate_scaffolding_include_templates=(sqerl.config)

db="postgresql://{{ cfg.db.user }}"
db="${db}:{{ cfg.db.password }}"
db="${db}@{{ cfg.db.host }}"
db="${db}:{{ #if bind.automate-pg-gateway }}{{ bind.automate-pg-gateway.first.port }}{{ else }}{{ cfg.db.port }}{{ /if }}"
db="${db}/{{ cfg.db.name }}_{{ cfg.rails_env }}"

redis="redis://{{ #if bind.automate-supermarket-redis }}{{ bind.automate-supermarket-redis.first.sys.ip }}{{ else }}{{ cfg.redis.host }}{{ /if }}"
redis="${redis}:{{ #if bind.automate-supermarket-redis }}{{ bind.automate-supermarket-redis.first.cfg.port }}{{ else }}{{ cfg.redis.port }}{{ /if }}"
redis="${redis}/{{ cfg.redis.database }}"

supermarket_path=$(hab pkg path chef/supermarket)

declare -A scaffolding_env
scaffolding_env[AIR_GAPPED]="{{ cfg.app.air_gapped_flag }}"
scaffolding_env[ANNOUNCEMENT_BANNER]="{{ cfg.app.announcement.banner_enabled }}"
scaffolding_env[ANNOUNCEMENT_TEXT]="{{ cfg.app.announcement.text }}"
scaffolding_env[API_ITEM_LIMIT]="{{ cfg.app.api_item_limit }}"
scaffolding_env[BACKTRACE]="{{ cfg.backtrace }}"
scaffolding_env[BUNDLE_GEMFILE]="$supermarket_path/app/Gemfile"
scaffolding_env[CDN_URL]="{{ cfg.s3.cdn_url }}"
scaffolding_env[CHEF_BLOG_URL]="{{ cfg.urls.chef_blog_url }}"
scaffolding_env[CHEF_DOCS_URL]="{{ cfg.urls.chef_docs_url }}"
scaffolding_env[CHEF_DOMAIN]="{{ cfg.urls.chef_domain }}"
scaffolding_env[CHEF_DOWNLOADS_URL]="{{ cfg.urls.chef_downloads_url }}"
scaffolding_env[CHEF_IDENTITY_URL]="{{ #if cfg.urls.chef_identity_url }}{{ cfg.urls.chef_identity_url }}{{ else }}{{ cfg.app.chef_server_url }}/id{{ /if }}"
scaffolding_env[CHEF_MANAGE_URL]="{{ #if cfg.urls.chef_manage_url }}{{ cfg.urls.chef_manage_url }}{{ else }}{{ cfg.app.chef_server_url }}{{ /if }}"
scaffolding_env[CHEF_OAUTH2_APP_ID]="{{ cfg.oauth2.app_id }}"
scaffolding_env[CHEF_OAUTH2_SECRET]="{{ cfg.oauth2.secret }}"
scaffolding_env[CHEF_OAUTH2_URL]="{{ #if cfg.oauth2.url }}{{ cfg.oauth2.url }}{{ else }}{{ cfg.app.chef_server_url }}{{ /if }}"
scaffolding_env[CHEF_OAUTH2_VERIFY_SSL]="{{ cfg.oauth2.verify_ssl }}"
scaffolding_env[CHEF_PROFILE_URL]="{{ #if cfg.urls.chef_profile_url }}{{ cfg.urls.chef_profile_url }}{{ else }}{{ cfg.app.chef_server_url }}{{ /if }}"
scaffolding_env[CHEF_SERVER_URL]="{{ cfg.app.chef_server_url }}"
scaffolding_env[CHEF_SIGN_UP_URL]="{{ #if cfg.urls.chef_sign_up_url }}{{ cfg.urls.chef_sign_up_url }}{{ else }}{{ cfg.app.chef_server_url }}/signup?ref=community{{ /if }}"
scaffolding_env[CHEF_STATUS_URL]="{{ cfg.urls.chef_status_url }}"
scaffolding_env[CHEF_TRAINING_URL]="{{ cfg.urls.chef_training_url }}"
scaffolding_env[CHEF_WWW_URL]="{{ cfg.urls.chef_www_url }}"
scaffolding_env[DATABASE_URL]="$db"
scaffolding_env[DATADOG_APP_NAME]="{{ cfg.datadog.app_name }}"
scaffolding_env[DATADOG_ENVIRONMENT]="{{ cfg.rails_env }}"
scaffolding_env[DATADOG_TRACER_ENABLED]="{{ cfg.datadog.tracer_enabled }}"
scaffolding_env[ENFORCE_PRIVACY]="{{ cfg.app.enforce_privacy }}"
scaffolding_env[FEATURES]="{{ strJoin cfg.app.features \", \" }}"
scaffolding_env[FIERI_FOODCRITIC_FAIL_TAGS]="{{ cfg.fieri.foodcritic_fail_tags }}"
scaffolding_env[FIERI_FOODCRITIC_TAGS]="{{ cfg.fieri.foodcritic_tags }}"
scaffolding_env[FIERI_KEY]="{{ cfg.fieri.key }}"
scaffolding_env[FIERI_SUPERMARKET_ENDPOINT]="http{{ #if cfg.nginx.force_ssl }}s{{ /if }}://localhost:{{ cfg.app.port }}"
scaffolding_env[FIERI_URL]="{{ cfg.fieri.url }}"
scaffolding_env[FIPS_ENABLED]="{{ cfg.fips.enabled }}"
scaffolding_env[FORCE_SSL]="{{ cfg.ssl.force_ssl }}"
scaffolding_env[FQDN]="{{ #if cfg.app.fqdn }}{{ cfg.app.fqdn }}{{ else }}{{ sys.hostname }}{{ /if }}"
scaffolding_env[FROM_EMAIL]="{{ cfg.app.from_email }}"
scaffolding_env[GITHUB_ACCESS_TOKEN]="{{ cfg.github.access_token }}"
scaffolding_env[GITHUB_CLIENT_OPTION_ACCESS_TOKEN_URL]="{{ cfg.github.access_token_url }}"
scaffolding_env[GITHUB_CLIENT_OPTION_AUTHORIZE_URL]="{{ cfg.github.option_authorize_url }}"
scaffolding_env[GITHUB_CLIENT_OPTION_SITE]="{{ cfg.github.option_site }}"
scaffolding_env[GITHUB_ENTERPRISE_URL]="{{ cfg.github.enterprise_url }}"
scaffolding_env[GITHUB_KEY]="{{ cfg.github.key }}"
scaffolding_env[GITHUB_SECRET]="{{ cfg.github.secret }}"
scaffolding_env[GITHUB_URL]="{{ cfg.github.url }}"
scaffolding_env[GOOGLE_ANALYTICS_ID]="{{ cfg.app.google_analytics_id }}"
scaffolding_env[LEARN_CHEF_URL]="{{ cfg.urls.learn_chef_url }}"
scaffolding_env[LOG_LEVEL]="{{ cfg.app.log_level }}"
scaffolding_env[NEWRELIC_AGENT_ENABLED]="{{ cfg.new_relic.enabled }}"
scaffolding_env[NEW_RELIC_APP_NAME]="{{ cfg.new_relic.app_name }}"
scaffolding_env[NEW_RELIC_APP_NAME]="{{ cfg.new_relic.app_name }}"
scaffolding_env[NEW_RELIC_LICENSE_KEY]="{{ cfg.new_relic.license_key }}"
scaffolding_env[OMNIBUS_FIPS_MODE]="{{ cfg.fips.omnibus_mode }}"
scaffolding_env[OMNIBUS_RPM_SIGNING_PASSPHRASE]="{{ cfg.app.rpm_signing_passphrase }}"
scaffolding_env[OPENSSL_FIPS]="{{ cfg.fips.openssl }}"
scaffolding_env[OWNERS_CAN_REMOVE_ARTIFACTS]="{{ cfg.app.owners_can_remove_artifacts }}"
scaffolding_env[PORT]="{{ #if cfg.nginx.force_ssl }}{{ cfg.nginx.ssl_port }}{{ else }}{{ cfg.nginx.non_ssl_port }}{{ /if }}"
scaffolding_env[PROGRESS_DOMAIN]="{{ cfg.urls.progress_domain }}"
scaffolding_env[PROGRESS_WWW_URL]="{{ cfg.urls.progress_www_url }}"
scaffolding_env[PROTOCOL]="http{{ #if cfg.nginx.force_ssl }}s{{ /if }}"
scaffolding_env[RAILS_LOG_TO_STDOUT]="{{ cfg.app.rails_log_to_stdout }}"
scaffolding_env[RAILS_SERVE_STATIC_FILES]="{{ cfg.app.serve_static_files }}"
scaffolding_env[REDIS_JOBQ_URL]="{{ cfg.redis.jobq_url }}"
scaffolding_env[REDIS_URL]="$redis"
scaffolding_env[ROBOTS_ALLOW]="{{ cfg.robots_allow }}"
scaffolding_env[ROBOTS_DISALLOW]="{{ cfg.robots_disallow }}"
scaffolding_env[S3_ACCESS_KEY_ID]="{{ cfg.s3.access_key_id }}"
scaffolding_env[S3_BUCKET]="{{ cfg.s3.bucket_name }}"
scaffolding_env[S3_DOMAIN_STYLE]="{{ cfg.s3.domain_style }}"
scaffolding_env[S3_ENCRYPTION]="{{ cfg.s3.encryption }}"
scaffolding_env[S3_ENDPOINT]="{{ cfg.s3.endpoint }}"
scaffolding_env[S3_PATH]="{{ cfg.s3.path }}"
scaffolding_env[S3_PRIVATE_OBJECTS]="{{ cfg.s3.private_objects }}"
scaffolding_env[S3_REGION]="{{ cfg.s3.region }}"
scaffolding_env[S3_SECRET_ACCESS_KEY]="{{ cfg.s3.secret_access_key }}"
scaffolding_env[S3_URLS_EXPIRE]="{{ cfg.s3.urls_expire }}"
scaffolding_env[SEGMENT_WRITE_KEY]="{{ cfg.app.segment_write_key }}"
scaffolding_env[SENTRY_URL]="{{ cfg.sentry_url }}"
scaffolding_env[STATSD_PORT]="{{ cfg.statsd_port }}"
scaffolding_env[STATSD_URL]="{{ cfg.statsd_url }}"
scaffolding_env[cookbook]="{{ cfg.app.cookbook }}"
scaffolding_env[ALLOWED_HOST]="{{ cfg.app.allowed_host }}"

scaffolding_env[INSTALL_DIRECTORY]="$supermarket_path"
scaffolding_env[INSTALL_PATH]="$supermarket_path"
scaffolding_env[APP_DIRECTORY]="$supermarket_path/app"
scaffolding_env[CONFIG_DIRECTORY]="{{ pkg.svc_config_path }}"
scaffolding_env[LOG_DIRECTORY]="{{ pkg.svc_var_path }}/log"
scaffolding_env[DATA_DIRECTORY]="{{ pkg.svc_data_path }}"
scaffolding_env[VAR_DIRECTORY]="{{ pkg.svc_var_path }}"
scaffolding_env[USER]="{{ pkg.svc_user }}"
scaffolding_env[GROUP]="{{ pkg.svc_group }}"
scaffolding_env[SECRET_KEY_BASE]='{{cfg.secret_key_base}}'

do_unpack() {
    return 0
}


do_setup_environment() {
  set_buildtime_env BUNDLE_SHEBANG "$(hab pkg path core/ruby27)/bin/ruby"
  set_buildtime_env BUNDLE_DEPLOYMENT true
  set_buildtime_env BUNDLE_JOBS "$(nproc)"
  set_buildtime_env BUNDLE_CLEAN false
  set_buildtime_env BUNDLE_PATH "$CACHE_PATH/vendor/bundle"
}

do_prepare(){

  # Override ruby version detection
  local gem_dir gem_path
  # The install prefix path for the app
  scaffolding_app_prefix="$supermarket_path/app"

  _detect_git

   # Determine Ruby engine, ABI version, and Gem path by running `ruby` itself.
  echo "#### RUBY PACKAGE ####"

  eval "$(hab pkg exec core/ruby27 ruby -- -r rubygems -rrbconfig - <<-'EOF'
    puts "local ruby_engine=#{defined?(RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'}"
    puts "local ruby_version=#{RbConfig::CONFIG['ruby_version']}"
    puts "local gem_path='#{Gem.path.join(':')}'"
EOF
  )"

  # Strip out any home directory entries at the front of the gem path.
  # shellcheck disable=SC2001
  gem_path=$(echo "$gem_path" | sed 's|^/root/\.gem/[^:]\{1,\}:||')
  # Compute gem directory where gems will be ultimately installed to
  echo "### RUBY VERSUIN ###"
  echo "$ruby_version and $ruby_engine"
  gem_dir="$scaffolding_app_prefix/vendor/bundle/$ruby_engine/$ruby_version"
  # Compute gem directory where gems are initially installed to via Bundler
  _cache_gem_dir="$CACHE_PATH/vendor/bundle/$ruby_engine/$ruby_version"

  # Silence Bundler warning when run as root user
  export BUNDLE_SILENCE_ROOT_WARNING=1

  # Attempt to preserve any original Bundler config by moving it to the side
  if [[ -f .bundle/config ]]; then
    build_line "Detecting existing bundler config. Temporarily renaming ..."
    mv .bundle/config .bundle/config.prehab
    dot_bundle=true
  elif [[ -d .bundle ]]; then
    dot_bundle=true
  fi

  GEM_HOME="$gem_dir"
  build_line "Setting GEM_HOME=$GEM_HOME"
  echo "#### GEM HOME ####"
  echo $GEM_HOME
  GEM_PATH="$gem_dir:$gem_path"
  build_line "Setting GEM_PATH=$GEM_PATH"
  
  echo "*** GEM PATH **** "
  echo $GEM_PATH
  export GEM_HOME GEM_PATH

  # core/ruby includes its own bundler which is used (2.2.22 at this time) however the scaffolding tries to vendor
  # core/bundler @ version 2.2.14
  _bundler_version="2.2.14"

}

do_build() {
  echo "INTO BUILD ### "
  scaffolding_bundle_install
}

do_install() {
  echo "INTO INSTAL ### "
  scaffolding_generate_binstubs
  scaffolding_vendor_bundler
  scaffolding_fix_binstub_shebangs
}

scaffolding_vendor_bundler() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  build_line "Vendoring 'bundler' version ${_bundler_version}"
  gem install \
    --local "$(hab pkg path core/bundler)/cache/bundler-${_bundler_version}.gem" \
    --install-dir "$GEM_HOME" \
    --bindir "$scaffolding_app_prefix/binstubs" \
    --no-document
  _wrap_ruby_bin "$scaffolding_app_prefix/binstubs/bundle"
  _wrap_ruby_bin "$scaffolding_app_prefix/binstubs/bundler"
}

scaffolding_generate_binstubs() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  build_line "Generating app binstubs in $scaffolding_app_prefix/binstubs"
  rm -rf "$scaffolding_app_prefix/.bundle"
  pushd "$scaffolding_app_prefix" > /dev/null
  _bundle_install \
    "$scaffolding_app_prefix/vendor/bundle" \
    --local \
    --quiet \
    --binstubs="$scaffolding_app_prefix/binstubs"
  popd > /dev/null
}

scaffolding_fix_binstub_shebangs() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  local shebang
  shebang="#!$(hab pkg path core/ruby27)/bin/ruby"

  build_line "Fixing Ruby shebang for binstubs"
  find "$scaffolding_app_prefix/binstubs" -type f | while read -r binstub; do
    if grep -q '^#!/usr/bin/env /.*/bin/ruby$' "$binstub"; then
      sed -e "s|^#!/usr/bin/env /.\{0,\}/bin/ruby\$|${shebang}|" -i "$binstub"
    fi
  done
}

scaffolding_bundle_install() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  local start_sec elapsed

  build_line "Installing dependencies using Bundler version ${_bundler_version}"
  start_sec="$SECONDS"

  {
    _bundle_install \
      "$scaffolding_app_prefix/vendor/bundle" \
      --retry 5
  } || {
      _restore_bundle_config
      e="bundler returned an error"
      exit_with "$e" 10
  }

  elapsed=$((SECONDS - start_sec))
  elapsed=$(echo $elapsed | awk '{printf "%dm%ds", $1/60, $1%60}')
  build_line "Bundle completed ($elapsed)"

  # If we preserved the original Bundler config, move it back into place
  if [[ -f .bundle/config.prehab ]]; then
    _restore_bundle_config
  fi
  # If not `.bundle/` directory existed before, then clear it out now
  if [[ -z "${dot_bundle:-}" ]]; then
    rm -rf .bundle
  fi
}

do_strip() {
    return 0
}

do_after() {
  touch "$pkg_prefix/config/app_env.sh"
  sed -i "s/{{cfg.db.name}}/{{ cfg.db.name }}_{{ cfg.rails_env }}/" "$pkg_prefix/config/app_env.sh"
  fix_interpreter "$pkg_prefix/app/bin/*" core/coreutils bin/env
  fix_interpreter "$pkg_prefix/app/exec/*" core/coreutils bin/env

  env_sh="$pkg_prefix/config/app_env.sh"
  rm -f "${env_sh}"
  for key in "${!scaffolding_env[@]}"; do
    if [[ "${scaffolding_env[$key]}" =~ ^\{\{[[:space:]](cfg\.[^[:space:]]+)[[:space:]]\}\}$ ]]; then
      echo "{{ #if ${BASH_REMATCH[1]} }}export $key='${scaffolding_env[$key]}'{{ /if }}" >> "$env_sh"
    else
      echo "export $key='${scaffolding_env[$key]}'" >> "$env_sh"
    fi
  done
}

_restore_bundle_config() {
  build_line "Restoring original bundler config"
  if [[ -f .bundle/config.prehab ]]; then
    rm -f .bundle/config
    mv .bundle/config.prehab .bundle/config
  fi
}

_detect_git() {
  if git rev-parse --is-inside-work-tree ; then
    build_line "Detected build is occuring inside a git work tree."
    _uses_git=true
    debug "Setting _uses_git to true."
  fi
}

_bundle() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  cd $scaffolding_app_prefix
  bundler_prefix="$(hab pkg path core/bundler)"
  echo " *** SETTING ENV *** "
  env \
    -u RUBYOPT \
    -u GEMRC \
    GEM_HOME="$bundler_prefix" \
    GEM_PATH="$bundler_prefix" \
    "$(hab pkg path core/ruby27)/bin/ruby" "$bundler_prefix/bin/bundle.real" ${*:-}
}

_bundle_install() {
  scaffolding_app_prefix="$(hab pkg path chef/supermarket)/app"
  cd $scaffolding_app_prefix
  local path
  path="$1"
  shift

echo "Going to install bundle *** "
  _bundle install ${*:-} \
    --jobs "$(nproc)" \
    --without development:test \
    --path "$path" \
    --shebang="$(hab pkg path core/ruby27)/bin/ruby" \
    --no-clean \
    --deployment
}

_wrap_ruby_bin() {
  echo "INTO WRAP RUBY BIN"
  local bin="$1"
  build_line "Adding wrapper $bin to ${bin}.real"
  mv -v "$bin" "${bin}.real"
  cat <<EOF > "$bin"
#!$(hab pkg path core/busybox-static)/bin/sh
set -e
if test -n "\$DEBUG"; then set -x; fi
export GEM_HOME="$GEM_HOME"
export GEM_PATH="$GEM_PATH"
unset RUBYOPT GEMRC
exec $(hab pkg path core/ruby27)/bin/ruby ${bin}.real \$@
EOF
  chmod -v 755 "$bin"
}

_create_process_bin() {
  local bin cmd env_sh
  bin="$1"
  cmd="$2"
  env_sh="$pkg_svc_config_path/app_env.sh"

  build_line "Creating ${bin} process bin"
  cat <<EOF > "$bin"
#!$(hab pkg path core/busybox-static)/bin/sh
set -e
if test -n "\$DEBUG"; then set -x; fi
export HOME="$pkg_svc_data_path"
if [ -f "$env_sh" ]; then
  . "$env_sh"
else
  >&2 echo "No app env file found: '$env_sh'"
  >&2 echo "Have you not started this service ($pkg_origin/$pkg_name) before?"
  >&2 echo ""
  >&2 echo "Aborting..."
  exit 1
fi
cd $scaffolding_app_prefix
exec $cmd \$@
EOF
  chmod -v 755 "$bin"
}