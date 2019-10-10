#shellcheck disable=SC2034
#shellcheck disable=SC2154

pkg_name=automate-cli
pkg_origin=chef
component_name=automate-cli
pkg_version="0.1.0"
pkg_bin_dirs=(bin)
pkg_build_deps=(core/coreutils)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_no_platform=true # Don't inject automate platform scaffolding
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/chef-automate"
)

do_after() {
  do_default_after

  # NOTE: don't change the callback. This checksuming needs to happen after all
  # the stripping happens, and do_after is that place.

  # The chef-automate "auto-updater" depends on the actual chef-automate binary
  # being placed in a specific location in the hab package. Therefore, instead
  # of relying on the scaffolding install we'll do it manually to please the
  # updater and chef-automate cli zip creator in expeditor.
  local bin_root="${pkg_prefix}/static/linux"
  mkdir -p "${bin_root}"

  for binary in "${scaffolding_go_binary_list[@]}"; do
    local bin
    bin=$(basename "$binary")
    local bin_path
    bin_path="${bin_root}/${bin}"
    local bin_sha_path
    bin_sha_path="${bin_path}.sha256sum"

    ln -sv "${pkg_prefix}/bin/${bin}" "${bin_path}"
    sha256sum "${bin_path}" | cut -d ' ' -f1 > "${bin_sha_path}"
  done
}
