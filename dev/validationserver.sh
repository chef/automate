#!/bin/bash

 while getopts ":S:K:" opt; do
    case $opt in
      N) chef_server_url="$OPTARG"
      ;;
      K) key_path="$OPTARG"
      ;;
      \?) echo "Invalid option -$OPTARG" >&2
      ;;
      : )
      echo "Invalid option: $OPTARG requires an argument" 1>&2
      ;;
    esac
  done

FLAG_VALUE="--key $key_path -u pivotal --config-option ssl_verify_mode=verify_none --config-option verify_api_cert=false"

knife raw -s $chef_server_url -m GET /organizations/ $FLAG_VALUE > orgs.json
json=$(cat orgs.json)
keys=($(echo "$json" | jq -r 'keys[]'))
total_orgs=${#keys[@]}
total_users= knife raw -s $chef_server_url -m GET /users/ $FLAG_VALUE | jq length

json_data="{ \"org_count\": \"$total_orgs\", \"user_count\": \"$total_users\"}"
my_array=("users" "environments")

#echo $keys
#echo $my_array
for org in "${keys[@]}";
do
  for str in "${my_array[@]}";
  do
     REQ_URL="/organizations/${org}/$str/"
     count= knife raw -s https://ec2-15-152-38-5.ap-northeast-3.compute.amazonaws.com -m GET $REQ_URL $FLAG_VALUE | jq length
     #echo $org
     echo "Fetched for $str"
     echo $count
   done
done