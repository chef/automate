#!/bin/bash

set -eo pipefail

cd /workdir/

data=$(curl --silent "https://a2-${CHANNEL}.cd.chef.co/assets/data.json")
instances_to_test=$(jq -nr --argjson data "$data" '$data[] | select(.id | startswith("single_local_all_")) | .fqdn')

for instance in ${instances_to_test[*]}
do
  echo "--- Executing Builder Tests Against $instance"
  test_bldr_url="https://$instance/bldr/v1"

  echo "--- Logging Into Builder"
  test_token="$(go run ./tools/builder-scaffolding login --url "$test_bldr_url" \
      --user admin --password chefautomate)"

  test_origin="testorigin-${RANDOM}${RANDOM}"
  echo "--- Creating Origin $test_origin"
  hab origin create -z "$test_token" -u "$test_bldr_url" "$test_origin"


  echo "--- Generating Origin Key"
  hab origin key generate "$test_origin"

  test_pkg="builder-test-plan"
  echo "--- Building Test Package $test_pkg"
  HAB_ORIGIN=$test_origin hab pkg build "integration/fixtures/test_plan/"

  echo "--- Uploading Test Package $test_origin/$test_pkg"
  hab pkg upload -z "$test_token" -u "$test_bldr_url" "results/$test_origin"*.hart

  echo "--- Downloading Test Package $test_origin/$test_pkg"
  test_dl_dir=$(mktemp -d --suffix="bldr_smoke")
  hab pkg download \
      -z "$test_token" \
      -u "$test_bldr_url" \
      -c "unstable" \
      --download-directory "$test_dl_dir" \
      "${test_origin}/${test_pkg}"
done
