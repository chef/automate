#!/bin/bash

# to run this script:
# A2_URL='https://a2-dev.test' A2_TOKEN='a token' components/compliance-service/scripts/delete-pg-data.sh

url=${A2_URL}
token=${A2_TOKEN}

echo "running deletion script for pg data against ${url} with ${token}"

declare -a jobs=$(curl -s --insecure -H "X-Data-Collector-Token: $token" $url/api/v0/compliance/scanner/jobs/search -d '{"per_page": 10000}' | jq '.jobs')

arr=( $(jq '.[].id' <<< "$jobs") )
for i in "${arr[@]}"; do
  temp="${i%\"}"
  id="${temp#\"}"
  echo "deleting job with id" $id
  curl -sSX DELETE -k -H "X-Data-Collector-Token: $token" $url/api/v0/compliance/scanner/jobs/id/$id
done

declare -a nodemanagers=$(curl -s --insecure -H "X-Data-Collector-Token: $token" $url/api/v0/nodemanagers/search -d '{}' | jq '.managers')

arr=( $(jq '.[].id' <<< "$nodemanagers") )
for i in "${arr[@]}"; do
  temp="${i%\"}"
  id="${temp#\"}"
  if [ "$id" != "e69dc612-7e67-43f2-9b19-256afd385820" ]; then
    echo "deleting nodemanager with id" $id
    curl -sSX DELETE -k -H "X-Data-Collector-Token: $token" $url/api/v0/nodemanagers/id/$id
  fi
done

declare -a nodes=$(curl -s --insecure -H "X-Data-Collector-Token: $token" $url/api/v0/nodes/search -d '{"per_page": 10000}' | jq '.nodes')

arr=( $(jq '.[].id' <<< "$nodes") )
for i in "${arr[@]}"; do
  temp="${i%\"}"
  id="${temp#\"}"
  echo "deleting node with id" $id
  curl -sSX DELETE -k -H "X-Data-Collector-Token: $token" $url/api/v0/nodes/id/$id
done

declare -a secrets=$(curl -s --insecure -H "X-Data-Collector-Token: $token" $url/api/v0/secrets/search -d '{}' | jq '.secrets')

arr=( $(jq '.[].id' <<< "$secrets") )
for i in "${arr[@]}"; do
  temp="${i%\"}"
  id="${temp#\"}"
  echo "deleting credential with id" $id
  curl -sSX DELETE -k -H "X-Data-Collector-Token: $token" $url/api/v0/secrets/id/$id
done
