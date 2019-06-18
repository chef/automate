#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

install_grpcurl() {
  hab pkg install core/grpcurl -b -f
}

install_grpcurl
