#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

install_grpcurl() {
  #grpcurl implicity depends on latest glibc
  hab pkg install core/glibc
  hab pkg install core/grpcurl -b -f
}

install_grpcurl
