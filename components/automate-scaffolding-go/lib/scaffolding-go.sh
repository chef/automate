#shellcheck disable=SC2034
#shellcheck disable=SC2154

do_default_before() {
  return 0
}

do_default_download() {
  return 0
}

do_default_clean() {
  return 0
}

do_default_verify() {
  return 0
}

do_default_unpack() {
  return 0
}

do_default_prepare() {
  GIT_SHA=$(git rev-parse HEAD)
  GO_LDFLAGS="-X github.com/chef/automate/lib/version.Version=${pkg_release}"
  GO_LDFLAGS="${GO_LDFLAGS} -X github.com/chef/automate/lib/version.GitSHA=${GIT_SHA}"
  GO_LDFLAGS="${GO_LDFLAGS} -X github.com/chef/automate/lib/version.BuildTime=${pkg_release}"
  export GO_LDFLAGS
}

do_default_build() {
  tmp_bin="${CACHE_PATH}/bin"
  mkdir -p "${tmp_bin}"

  local go_cmd
  go_cmd="GOBIN=${tmp_bin} go install -mod=vendor"

  # Default to a static build unless the user plan has defined the
  # scaffolding_go_no_static variable.
  if [[ -z ${scaffolding_go_no_static} ]]; then
    # Go doesn't currently have an "easy" way to enforce static builds. Until the
    # proposed `-static` flag is added to go-build/go-install, we'll need to add
    # the proper environment variables, ldflags and tags to have reasonable
    # assurance that our build will be static.
    build_line "Enabling static build"

    # We assume no CGO here. While we could probably go to some length to leave
    # CGO enabled and use musl, it's probably best for projects that would need
    # musl to override this hook and build it as necessary.
    go_cmd="CGO_ENABLED=0 ${go_cmd}"

    GO_LDFLAGS="${GO_LDFLAGS} -extldflags \"-fno-PIC -static\""

    if [[ -z ${scaffolding_go_build_tags} ]]; then
      declare -a scaffolding_go_build_tags=()
    fi
    scaffolding_go_build_tags+=('osusergo netgo static_build')
    unique_tags=$(
      for i in "${scaffolding_go_build_tags[@]}"; do
        echo "${i}";
      done | uniq
    )
    scaffolding_go_build_tags=("${unique_tags}")
  fi

  # Inject Go ldflags
  if [[ ${GO_LDFLAGS} ]]; then
    go_cmd="${go_cmd} --ldflags '${GO_LDFLAGS}'"
  fi

  # Inject Go build tags
  if [[ ${scaffolding_go_build_tags[*]} ]]; then
    go_cmd="${go_cmd} --tags '${scaffolding_go_build_tags[*]}'"
  fi

  # Build all the things
  pushd "${SRC_PATH}" > /dev/null || exit 1
    if [[ -n ${scaffolding_go_binary_list} ]]; then
      for binary in "${scaffolding_go_binary_list[@]}"; do
        local _build_cmd
        _build_cmd="${go_cmd} $binary"
        build_line "Building: ${_build_cmd}"
        eval "${_build_cmd}"
      done
    else
      build_line "Building: ${go_cmd}"
      eval "${go_cmd}"
    fi
  popd >/dev/null || exit 1
}

do_default_install() {
  # Avoids the need to have pkg_bin_dirs=(bin) on every plan
  mkdir -p "${pkg_prefix}/bin/"

  if [[ ${scaffolding_go_binary_list} ]]; then
    for binary in "${scaffolding_go_binary_list[@]}"; do
      base=$(basename "${binary}")
      mv "${tmp_bin}/${base}" "${pkg_prefix}/bin/"
    done
  else
    mv "${tmp_bin}/${pkg_name}" "${pkg_prefix}/bin/"
  fi
}

do_check() {
  # Ensure that the go binaries are static if we've set the flag
  if ! [[ ${scaffolding_go_no_static} ]]; then
    check_static_binary() {
      build_line "Checking for dynamic links in ${1}"
      if ! ldd "${1}" | grep "not a dynamic executable"; then
          exit_with "${binary} is not a static executable" 1
      fi
    }

    if [[ ${scaffolding_go_binary_list} ]]; then
      for binary in "${scaffolding_go_binary_list[@]}"; do
        base=$(basename "$binary")
        check_static_binary "${tmp_bin}/${base}"
      done
    else
      check_static_binary "${tmp_bin}/${pkg_name}"
    fi
  fi

  exit 0
}

do_default_after() {
  rm -rf "${tmp_bin:?}/*"
}
