#!/bin/bash

# to run this script:
#    export A2_URL='https://a2-dev.test'
#    export A2_TOKEN='SOME-TOKEN'
#    export AUTOMATE_ACCEPTANCE_TARGET_HOST=some-host
#    export AUTOMATE_ACCEPTANCE_TARGET_USERNAME=some-host
#    export AUTOMATE_ACCEPTANCE_TARGET_PASSWORD=some-password
#    components/compliance-service/scripts/create-pg-data.sh

url=${A2_URL}
token=${A2_TOKEN}
node_count=10
job_count=2

echo "running data generation script for pg data against ${url} with ${token}"

# create the ssh secrets
echo "creating ssh secret for vagrant"
ssh_secret_vagrant=$(curl -s --insecure -H "api-token: $token" $url/api/v0/secrets -d '{
  "name": "my ssh secret",
  "type": "ssh",
  "data": [
    { "key": "username", "value": "vagrant" },
    { "key": "password", "value": "vagrant"}
  ]
}'  | jq '.id')

echo $ssh_secret_vagrant

echo "creating ssh secret for ec2 instance"
ssh_secret_ec2=$(curl -s --insecure -H "api-token: $token" $url/api/v0/secrets -d '{
  "name": "my ssh secret",
  "type": "ssh",
  "data": [
    { "key": "username", "value": "$AUTOMATE_ACCEPTANCE_TARGET_USERNAME" },
    { "key": "password", "value": "$AUTOMATE_ACCEPTANCE_TARGET_PASSWORD" }
  ]
}'  | jq '.id')

echo $ssh_secret_ec2


# add nodes
for i in $(seq 1 $((node_count/2))); do
  echo "creating node" $i
  vagrant_node_id=$( curl -s --insecure -H "api-token: $token" $url/api/v0/nodes -d '{
    "name": "my-vagrant-node",
    "manager":"automate",
      "target_config": {
        "backend":"ssh",
        "host":"localhost",
        "secrets":['${ssh_secret_vagrant}'],
        "port": 22
      },
      "tags": [
        { "key":"test-node", "value":"is amazing" },
        { "key":"compliance-service", "value":"rockin like whoa" },
        { "key": "_no_auto_detect", "value": "true"}
      ]
  }' | jq '.id'); echo $vagrant_node_id; done

for i in $(seq 1 $((node_count/2))); do
  echo "creating node" $i
  ec2_node_id=$( curl -s --insecure -H "api-token: $token" $url/api/v0/nodes -d '{
    "name": "my-ssh-node",
    "manager":"automate",
      "target_config": {
        "backend":"ssh",
        "host":"$AUTOMATE_ACCEPTANCE_TARGET_HOST",
        "secrets":['${ssh_secret_ec2}'],
        "port": 22
      },
      "tags": [
        { "key":"test-node", "value":"is amazing" },
        { "key":"compliance-service", "value":"rockin like whoa" },
        { "key": "_no_auto_detect", "value": "true"}
      ]
  }' | jq '.id'); echo $ec2_node_id; done

# upload a profile
curl --insecure -F file=@components/compliance-service/api/tests/mario-0.1.0.tar.gz -H "api-token: $token"  $url/api/v0/compliance/profiles?owner=admin

# add jobs
for i in $(seq 1 ${job_count}); do
  echo "creating job" $i
  echo $(curl -s --insecure -H "api-token: $token" $url/api/v0/compliance/scanner/jobs -d '{
      "name": "my job",
      "tags": [],
      "type": "exec",
      "nodes": ['${vagrant_node_id}', '${ec2_node_id}'],
      "profiles": ["https://github.com/dev-sec/linux-baseline/archive/master.tar.gz", "https://github.com/dev-sec/ssh-baseline/archive/master.tar.gz"],
      "retries": 1,
      "node_selectors": []
  }' | jq '.id'); done

# example of adding a nodemanager
# curl -s --insecure -H "api-token: $token" $url/api/v0/nodemanagers -d '{
#     "name": "my aws api integration with session token",
#     "type": "aws-api",
#     "instance_credentials": [],
# 		"credential_data": [
# 			{Key: "AWS_ACCESS_KEY_ID", Value: "value" },
# 			{Key: "AWS_SECRET_ACCESS_KEY", Value: "value" },
# 			{Key: "AWS_SESSION_TOKEN", Value: "value" }
# 		]
#   }'
