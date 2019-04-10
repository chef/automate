#!/bin/bash

set -euo pipefail

dns_suffix=${DNS_SUFFIX:-dev}
tfstate=${TF_STATE:-acceptance.tfstate}

# This is a command that will:
#   a) read tfstate from S3
#   b) parse it
#   c) write it to a data.json file on S3
# all without saving anything to disk
echo "Extracting data from $tfstate and putting it in a2-$dns_suffix.cd.chef.co"

aws --profile chef-cd s3 cp s3://chef-cd-terraform-state/a2/$tfstate - | ruby .expeditor/parse-tfstate.rb | aws --profile chef-cd s3 cp - s3://a2-$dns_suffix.cd.chef.co/assets/data.json
