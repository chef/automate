#!/bin/bash
#
# The script reads bindings from our component plan files and emits a
# dot representation to make it easier to visualize the dependencies.
#
# You can generate a png like this:
# ./scripts/binding_deps.sh | dot -Tpng -obindings.png
#
if [ "${BASH_VERSINFO[0]}" -lt 4 ]
then
  echo "Minimum version of bash 4 is required. Found ${BASH_VERSINFO[0]}."
  echo "Try running inside the hab studio instead."
  exit 1
fi

ROOT_DIR="$(dirname "${BASH_SOURCE[0]}")/.."

bindings() {
  for component_path in $(find "$ROOT_DIR/components/"* -maxdepth 0 -type d | sort)
  do
    local component
    component=$(basename "${component_path}")
    # We'll check anything with a runhook
    if [[ -e "${component_path}/habitat/hooks/run" ]]
    then
      (
        declare -A pkg_binds
        declare -A pkg_binds_optional
        #shellcheck disable=SC1090
        source "${component_path}/habitat/plan.sh"
        safe_comp=${component//-/_}
        if [[ -n "${!pkg_binds[*]}" ]]
        then
            readarray -t sorted < <(printf '%s\n' "${!pkg_binds[@]}" | sort)
            for b in "${sorted[@]}"
            do
                echo "${safe_comp} -> ${b//-/_}"
            done
        fi

        if [[ -n "${!pkg_binds_optional[*]}" ]]; then
            readarray -t sorted < <(printf '%s\n' "${!pkg_binds_optional[@]}" | sort)
            for b in "${sorted[@]}"; do
                echo "${safe_comp} -> ${b//-/_}[color=grey]"
            done
        fi
      ) 2> /dev/null
    fi
  done
}

read -r -d '' HEADER <<'DOC'
digraph "binding_map" {

DOC

echo "${HEADER}"
bindings
echo "}"
