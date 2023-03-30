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
  chef/mlsa
  core/openssl
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

do_prepare() {
  echo "Do prepare hook called *********"
  build_line "Setting link for /usr/bin/env to 'coreutils'"
  [[ ! -f /usr/bin/env ]] && ln -s "$(pkg_path_for coreutils)/bin/env" /usr/bin/env
  return 0
}

do_download() {
  echo "Download hook called *******"
  return 0
}

do_build() {
  echo "Build hook called *******"
  return 0
}

do_install() {
  echo "Install hook called *******"
  # chmod 777 -R /hab/pkgs/core/ruby27/2.7.5/20220312100031/lib/ruby/gems/2.7.0
  # chmod 777 -R /hab/pkgs/chef/oc_id/15.4.0/20230105061030/oc_id
  export DATABASE_URL="postgresql://automate@127.0.0.1:5432/automate-cs-ocid?sslmode=verify-ca&sslcert=/hab/svc/automate-postgresql/config/server.crt&sslkey=/hab/svc/automate-postgresql/config/server.key&sslrootcert=/hab/svc/automate-postgresql/config/root.crt"
  export RUBY_BIN_DIR="/hab/pkgs/core/ruby27/2.7.5/20220312100031/bin"
  export PATH=$PATH:$RUBY_BIN_DIR
  echo "********RUBYFILE PATH"
  echo $PATH

  cd /hab/pkgs/chef/oc_id/15.4.0/20230105061030/oc_id
  echo "gem 'tzinfo-data'" >> Gemfile
  ########
  echo "gem 'thin'" >> Gemfile
  mkdir -p .ssl
  rm -rf .ssl/*
  openssl req -new -newkey rsa:2048 -sha1 -days 365 -nodes -x509 -keyout .ssl/localhost.key -out .ssl/localhost.crt
  ########
  bundle package --no-install
  bundle install --path=vendor/bundle

  echo "******Creating Database"
  bundle exec bin/rake db:create

  echo "******Running Migrations"
  bundle exec bin/rake db:migrate
  mkdir -p tmp
  chmod 777 -R tmp
  # chmod 777 -R .bundle
  return 0
}


