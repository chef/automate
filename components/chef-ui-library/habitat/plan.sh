pkg_name=ui-library
pkg_origin=chef
pkg_version="0.1.0"
pkg_svc_user="root"

pkg_deps=(
  core/nginx
  core/coreutils
)

pkg_build_deps=(
  core/node10/"$(cat "$PLAN_CONTEXT/../.nvmrc")"
)

do_build() {
  npm install --production

  for b in node_modules/.bin/*; do
    fix_interpreter "$(readlink -f -n "$b")" core/coreutils bin/env
  done

  npm run build
}

do_install() {
  mkdir -pv $pkg_prefix/www
  cp -Rv www/* $pkg_prefix/www
}
