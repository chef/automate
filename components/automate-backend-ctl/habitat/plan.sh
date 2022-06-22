#stable channel

pkg_name="automate-ha-ctl"
pkg_origin="chef"
pkg_version="0.1.0"
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=("Chef-MLSA")
pkg_description="Provides automate-backend-ctl for Automate HA Backend Services"

do_before() {
  update_pkg_version
}

pkg_deps=(
  core/ruby30
  core/libffi
  chef/mlsa
  core/bash
  core/coreutils
  core/glibc
  core/postgresql-client
)

pkg_build_deps=(
  core/make
  core/gcc
  core/rsync
)

pkg_bin_dirs=(bin)

do_strip() {
  return 0
}

do_build() {
  return 0
}

do_before() {
  update_pkg_version
}

do_prepare() {
  gem update --system --no-document
  gem install bundler -v "$(grep -A 1 "BUNDLED WITH" $PLAN_CONTEXT/Gemfile.lock | tail -n 1)"

  export GEM_HOME="$pkg_prefix/vendor/bundle/ruby/3.0.0"
  build_line "Setting GEM_HOME='$GEM_HOME'"
  export GEM_PATH="$GEM_HOME"
  build_line "Setting GEM_PATH='$GEM_PATH'"
}

do_unpack() {
  mkdir -pv "$HAB_CACHE_SRC_PATH/$pkg_dirname"
  cp -RT "$PLAN_CONTEXT"/ "$HAB_CACHE_SRC_PATH/$pkg_dirname/"
}

do_build() {
  pushd "$HAB_CACHE_SRC_PATH/$pkg_dirname/" > /dev/null
    bundle config binstubs binstubs
    bundle install --path "$HAB_CACHE_SRC_PATH/$pkg_dirname/vendor/bundle"
  popd
}

do_install() {
  # Copy component files
  rsync -a --exclude habitat --exclude results "$HAB_CACHE_SRC_PATH/$pkg_dirname/" "${pkg_prefix}/"

  # Install wrapper 'binaries'
  wrapper_bin_path="${pkg_prefix}/bin"
  install "$PLAN_CONTEXT/bin/automate-backend-ctl" "$wrapper_bin_path/automate-backend-ctl"

  # Fix up some paths
  build_line "Replacing REPLACE_ME with ${pkg_prefix} in ${pkg_prefix}/bin/automate-backend-ctl"
  sed -i -e "s,REPLACE_ME,${pkg_prefix},g" "${pkg_prefix}/bin/automate-backend-ctl"
  #fix_interpreter "${pkg_prefix}/bin/automate-backend-ctl" core/bash bin/bash
  fix_interpreter "${pkg_prefix}/bin/automate-backend-ctl" core/coreutils bin/env
  build_line "Replacing REPLACE_ME with ${pkg_prefix} in ${pkg_prefix}/bin/knife"
  sed -i -e "s,REPLACE_ME,${pkg_prefix},g" "${pkg_prefix}/bin/knife"
  fix_interpreter "${pkg_prefix}/bin/knife" core/bash bin/bash

  wrap_ruby_bin "$pkg_prefix/bin/automate-backend-ctl"
  wrap_ruby_bin "$pkg_prefix/bin/knife"
}

wrap_ruby_bin() {
  local bin="$1"
  build_line "Adding wrapper $bin to ${bin}.real"
  mv -v "$bin" "${bin}.real"
  cat <<EOF > "$bin"
#!$(pkg_path_for busybox-static)/bin/sh
set -e
if test -n "\$DEBUG"; then set -x; fi
export GEM_HOME="$GEM_HOME"
export GEM_PATH="$GEM_PATH"
unset RUBYOPT GEMRC
exec $(pkg_path_for ruby30)/bin/ruby -I $pkg_prefix/lib ${bin}.real \$@
EOF
  chmod -v 755 "$bin"
}
