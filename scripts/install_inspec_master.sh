#!/bin/bash
set -euo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

install_inspec() {
  hab pkg install core/gcc --binlink --force
  hab pkg install core/ruby --binlink --force

  tmp_dir=$(mktemp -d)
  git clone --depth=1 https://github.com/inspec/inspec "$tmp_dir"
  cd "$tmp_dir"
  bundle
  bundle exec rake install
}

install_inspec
