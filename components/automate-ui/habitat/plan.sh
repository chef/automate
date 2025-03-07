# stable channel

pkg_name=automate-ui
pkg_origin=chef
pkg_version="2.0.0"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_svc_user="root" # so we can start nginx properly
pkg_deps=(
  core/coreutils/8.32/20240105213308
  core/curl/8.7.1/20240614090648
  chef/mlsa/1.0.1/20240125084021
  core/nginx/1.25.4/20240314072919
  core/jq-static/1.6/20240107004905
)

pkg_build_deps=(
  core/git/2.33.1/20240614092831
  core/make/4.3/20240105222044
  #core/node18/"$(cat "$PLAN_CONTEXT/../.nvmrc")"
  core/node18/18.19.1/20240301044734
  core/rsync/3.2.3/20240107034222
)


pkg_exports=(
  [port]=service.port
  [host]=service.host
)
pkg_exposes=(port)
pkg_description="Web interface for Chef Automate Visibility"
pkg_upstream_url="https://www.chef.io/automate/"

do_unpack() {
  # Copy the project files into the cache src path so we can have a clean
  # node_modules and only the files we need
  mkdir -p $CACHE_PATH/automate-ui
  mkdir -p $CACHE_PATH/chef-ui-library
  rsync --archive --exclude node_modules $PLAN_CONTEXT/.. $CACHE_PATH/automate-ui
  rsync --archive --exclude node_modules $PLAN_CONTEXT/../../chef-ui-library $CACHE_PATH
}

npm_install() {
  # --unsafe-perm enables the package.json install task to copy files when running
  # as superuser during the hab package building.
  # Copied from Habitat's node scaffolding:
  # https://github.com/habitat-sh/core-plans/blob/be88f083c123ab998711fd3a93976ad10492a955/scaffolding-node/lib/scaffolding.sh#L111-L116
  npm install \
    --unsafe-perm \
    --loglevel error \
    --fetch-retries 5 \
    "$@"
}

fix_interpreters() {
  # Fix the interpreters of the binaries
  # Note: many bin/* files are links, so the output will have duplicate entries
  for b in node_modules/.bin/*; do
    fix_interpreter "$(readlink -f -n "$b")" core/coreutils bin/env
  done
}

do_build() {
  # Disabling Usage Analytics
  export NG_CLI_ANALYTICS=false
  export IBM_TELEMETRY_DISABLED='true'

  echo "Building $CACHE_PATH/chef-ui-library"
  pushd "$CACHE_PATH/chef-ui-library"
    npm_install
    fix_interpreters
    npm run build:prod
  popd

  echo "Building $CACHE_PATH/automate-ui"
  pushd "$CACHE_PATH/automate-ui"
    npm_install --production

    # Angular CLI isn't included in production deps so we need to install it manually.
    npm_install @angular/cli

    fix_interpreters
    npm run install:ui-library
    npm run build:prod

    npm uninstall @angular/cli --no-save
  popd
}

do_install() {
  cp -R $CACHE_PATH/automate-ui/dist "$pkg_prefix"
}

do_after() {
  rm -rf ~/.netrc
}


