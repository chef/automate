pkg_name=automate-scaffolding
pkg_origin=core
pkg_description="Generic scaffolding for Automate Applications internally at Chef Software Inc."
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_version="0.1.0"
pkg_license=('Chef-MLSA')
pkg_source=nosuchfile.tar.gz

do_build() {
  return 0
}

do_verify() {
  return 0
}

do_unpack() {
  return 0
}

do_download() {
  return 0
}

# Install scaffolding libraries from the plan into the package.
do_install() {
  install -D -m 0644 "$PLAN_CONTEXT/../lib/scaffolding.sh" "$pkg_prefix/lib/scaffolding.sh"
  install -D -m 0644 "$PLAN_CONTEXT/../../../lib/scaffolding/shared.sh" "$pkg_prefix/lib/shared.sh"
  install -d "$pkg_prefix/lib/support/"
  install -D -m 0644 "$PLAN_CONTEXT/../../../lib/scaffolding/support/"* "$pkg_prefix/lib/support/"
  install -d "$pkg_prefix/lib/templates/"
  install -D -m 0644 "$PLAN_CONTEXT/../../../lib/scaffolding/templates/"* "$pkg_prefix/lib/templates/"
}
