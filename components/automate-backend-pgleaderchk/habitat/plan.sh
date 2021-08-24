# shellcheck disable=SC2148
pkg_name=automate-backend-pgleaderchk
binary_name=pgleaderchk
pkg_origin=chef
pkg_repo=a2-ha-backend
pkg_version="0.1.0"
pkg_description="Automate Backend PostreSQL leader check"
pkg_maintainer="Chef Software Inc. <support@chef.io>"
pkg_license=('Chef-MLSA')
pkg_upstream_url="http://github.com/chef/automate/components/automate-backend-pgleaderchk"
pkg_deps=(
  core/bash
  core/curl
  core/jq-static
  chef/mlsa
)
pkg_build_deps=(
  core/gcc
)
pkg_exports=(
  [port]=httpd.port
)
pkg_binds_optional=(
  [database]="port"
  [database]="ssl"
)
pkg_exposes=(port)


pkg_bin_dirs=(bin)
pkg_scaffolding="${local_scaffolding_origin:-chef}/automate-scaffolding-go"
scaffolding_go_base_path=github.com/chef
scaffolding_go_repo_name=automate
scaffolding_go_import_path="${scaffolding_go_base_path}/${scaffolding_go_repo_name}/components/${pkg_name}"
scaffolding_go_binary_list=(
  "${scaffolding_go_import_path}/cmd/${pkg_name}"
)



#pkg_bin_dirs=(bin)
#pkg_scaffolding=core/scaffolding-go
#scaffolding_go_base_path=github.com/chef
#scaffolding_go_build_deps=(
#  "github.com/lib/pq"
#  "github.com/pkg/errors"
#  "github.com/sirupsen/logrus"
#  "github.com/spf13/cobra"
#  "github.com/spf13/viper"
#)

#pkg_version() {
#  cat "$PLAN_CONTEXT/../../../VERSION"
#}

#do_before() {
#  build_line "Overriding scaffolding do_default_before"
#  export scaffolding_go_pkg_path="$scaffolding_go_workspace_src/$scaffolding_go_base_path/$pkg_repo"
  # Initialize the Go Workspace package path if we are tryng to build the
  # package from local /src, that is when there is no $pkg_source set.
 # if [[ ! $pkg_source ]]; then
 #   mkdir -p "$scaffolding_go_workspace_src/$scaffolding_go_base_path"
 #   ln -sf /src "$scaffolding_go_pkg_path"
 # fi

 # if [ ! -f "$PLAN_CONTEXT/../../../VERSION" ]; then
 #   exit_with "Cannot find VERSION file! You must enter the studio from the project's top-level directory." 56
 # fi
  # update_pkg_version
# }

#scaffolding_go_get_with_flags() {
#  local deps
#  deps=($pkg_source ${scaffolding_go_build_deps[@]})
#  build_line "Downloading Go build dependencies"
#  if [[ "${#deps[@]}" -gt 0 ]] ; then
#    for dependency in "${deps[@]}" ; do
#      go get --ldflags "${GO_LDFLAGS}" "$(_sanitize_pkg_source "$dependency")"
#    done
#  fi
#}

#do_download(){
 # export GO_COMPONENT_IMPORT_PATH="${scaffolding_go_base_path}/$pkg_repo/components/${pkg_name}"
 # export GOBIN=$GOPATH/bin
 # export PATH=$GOBIN:$PATH
 # build_line "Overriding scaffolding do_default_download"
 # scaffolding_go_get_with_flags
# }

#do_prepare() {
#  export BUILD_TIME
#  BUILD_TIME=$(date -u '+%Y-%m-%d_%H:%M:%S')
#  LINKER_FLAGS=" -X $GO_COMPONENT_IMPORT_PATH/pkg/version.Version=$pkg_version"
 # LINKER_FLAGS="$LINKER_FLAGS -X $GO_COMPONENT_IMPORT_PATH/pkg/version.BuildTime=$BUILD_TIME"
 # export LINKER_FLAGS
#}

# do_build() {
  #build_line "Overriding Build process"
  #pushd "$scaffolding_go_pkg_path" >/dev/null
  #go install --ldflags "${GO_LDFLAGS} ${LINKER_FLAGS}" "${GO_COMPONENT_IMPORT_PATH}/cmd/${binary_name}"
  #popd >/dev/null
# }

do_install() {
  do_default_install
  build_line "Overriding Install process"
  cp -r "${GOBIN}/$binary_name" "${pkg_prefix}/bin"
}

do_strip() {
  return 0
}
