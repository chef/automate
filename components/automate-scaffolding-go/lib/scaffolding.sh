#shellcheck disable=SC2154

# Don't load the shared config if the scaffolding_no_platform variable is set.
if [[ -z "${scaffolding_no_platform}" ]]; then
  build_line "Using shared platform scaffolding"
  #shellcheck source=../../../lib/scaffolding/shared.sh
  source "$(pkg_path_for "${pkg_scaffolding}")/lib/shared.sh"
fi

build_line "Using automate go scaffolding"
#shellcheck source=../lib/scaffolding-go.sh
source "$(pkg_path_for "${pkg_scaffolding}")/lib/scaffolding-go.sh"
