#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

ident="${1?ident required}"
hab pkg install -b "$ident"
