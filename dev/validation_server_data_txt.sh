#!/bin/bash

 while getopts ":S:K:F:" opt; do
    case $opt in
      S) chef_server_url="$OPTARG"
      ;;
      K) key_path="$OPTARG"
      ;;
      F) file_name="$OPTARG"
      ;;
      \?) echo "Invalid option -$OPTARG" >&2
      ;;
      : )
      echo "Invalid option: $OPTARG requires an argument" 1>&2
      ;;
    esac
  done

FLAG_VALUE="--key $key_path -u pivotal --config-option ssl_verify_mode=verify_none --config-option verify_api_cert=false"

orgs=($(knife raw -s $chef_server_url -m GET /organizations/ $FLAG_VALUE | jq --sort-keys | jq -r 'keys_unsorted[]'))
total_orgs=${#orgs[@]}
total_users=($(knife raw -s $chef_server_url -m GET /users/ $FLAG_VALUE | jq length))
echo "org_count : $total_orgs" > $file_name
echo "user_count : $total_users" >> $file_name

#json_data="{ \"org_count\": \"$total_orgs\", \"user_count\": \"$total_users\"}"
attributes=("users" "environments" "nodes" "cookbooks" "policies" "data" "roles" "clients")


for org in "${orgs[@]}";
do
  echo "Data for organisation : ${org}" >> $file_name
  for field in "${attributes[@]}";
  do
     REQ_URL="/organizations/${org}/$field/"
     echo "Fetching for url $REQ_URL"
     count=($(knife raw -s $chef_server_url -m GET $REQ_URL $FLAG_VALUE | jq length))
     echo "count is " $count
     key=$field"_count"
     value=$count
     echo $key":"$value >> $file_name
     echo $my_array
   done
   #json=$(echo ${my_array[i@]} | jq -n 'reduce inputs as $i ({}; . + $i | {(.): null})')
   echo $json
done