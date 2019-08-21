#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

install_gcc() {
  hab pkg install core/gcc -b -f
}

install_gcc