#!/bin/bash

 while getopts ":S:K:F:" opt; do
    case $opt in
      S) chef_server_url="$OPTARG"
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

orgs=($(knife raw -s $chef_server_url -m GET /organizations/ $FLAG_VALUE | jq -r 'keys[]'))
total_orgs=${#orgs[@]}
total_users= knife raw -s $chef_server_url -m GET /users/ $FLAG_VALUE | jq length

json_data="{ \"org_count\": \"$total_orgs\", \"user_count\": \"$total_users\"}"
attributes=("users" "environments")

declare -A my_array

for org in "${orgs[1]}";
do
  for field in "${attributes[@]}";
  do
     REQ_URL="/organizations/${org}/$field/"
     echo "Fetching for url $REQ_URL"
     count=($(knife raw -s $chef_server_url -m GET $REQ_URL $FLAG_VALUE | jq length))
     echo "count is " $count
     key=$field"_count"
     value=$count
     echo $key $value
     my_array["$key"]=$value
     echo $my_array
   done
   jq -n --argjson n "${#my_array[@]}" '{ ${org}:
                (
                        reduce range($n) as $i ({};
                                .[$ARGS.positional[$i]] = ($ARGS.positional[$i+$n])
                        )
                )
         }' --args "${!my_array[@]}" "${my_array[@]}"
   #json=$(echo ${my_array[i@]} | jq -n 'reduce inputs as $i ({}; . + $i | {(.): null})')
   echo $json
done