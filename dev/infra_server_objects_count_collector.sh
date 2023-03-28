#!/bin/bash

usage()
{
cat << EOF
usage: $0 options

This script gets the count of the chef server objects using the knife commands(via chef API). The chef server objects are:
1. Organisations
2. Users
3. Nodes
4. Environemnts
5. Cookbooks
6. Roles
7. Policy
8. Policy Groups
9. Clients

All the counts are saved in a toml file for validation.

OPTIONS:
   -h | --help    Shows the help message
   -S             Chef Server url (Eg: https://chef-server.example.com).
   -K             Path to the pivotal user key
   -F             Output file path.
EOF
}

check_binary() {
  if ! which "$1" > /dev/null; then
    ( >&2 echo "$2" )
    # Exit with a nonzero code so that the caller knows the script failed.
    exit 1
  fi
}



 while getopts ":hS:K:F:-help" opt; do
    case $opt in
      h|help) usage
        exit 1
      ;;
      S) chef_server_url="$OPTARG"
      ;;
      K) key_path="$OPTARG"
      ;;
      F) file_name="$OPTARG"
      ;;
      \?) echo "Invalid option -$OPTARG" >&2
        usage
        exit 1
      ;;
      :) echo "Invalid option: $OPTARG requires an argument" >&2
        usage
        exit 1
      ;;
    esac
  done


if [ -z "$key_path" ]; then
  echo "Missing Pivotal Key Path " >&2
  usage
  exit 1
fi

if [ -z "$file_name" ]; then
  echo "Missing Output File Path" >&2
  usage
  exit 1
fi

#Checking if binary exists for knife otherwise asking user to install knife via workstation
check_binary "knife" "$(cat <<EOF
You will need knife to run this script.
Please follow the document https://docs.chef.io/workstation/install_workstation/ to install knife via workstation
EOF
)"

#Checking if binary exists for jq otherwise installing it on the machine
check_binary "jq" "$(cat <<EOF 
You will need jq to run this script.
Install it using your package manager. 
For homebrew:
brew install jq
For linux/Ubuntu:
sudo apt-get install jq
For Debian:
sudo apt install jq -y
EOF
)"

if [ -z "$chef_server_url" ]; then
FLAG_VALUE="--key $key_path -u pivotal --config-option ssl_verify_mode=verify_none --config-option verify_api_cert=false"
else 
 FLAG_VALUE="--key $key_path -u pivotal --config-option ssl_verify_mode=verify_none --config-option verify_api_cert=false -s $chef_server_url"
fi


#### Flow if the url is needed ####
echo "Getting count of the organizations"
orgs=($(knife raw -m GET /organizations/ $FLAG_VALUE | jq --sort-keys | jq -r 'keys_unsorted[]'))
total_orgs=${#orgs[@]}

echo "Getting count of the users"
total_users=($(knife raw -m GET /users/ $FLAG_VALUE | jq length))

cat << EOF > "$file_name"
total_orgs_count=$total_orgs
total_users_count=$total_users
EOF

attributes=("users" "environments" "nodes" "cookbooks" "policies" "data" "roles" "clients" "policy_groups")

for org in "${orgs[@]}";
do
  echo "Getting objects count of the organization: $org"
  echo "[[organisations]]" >> $file_name
  echo "name=\"$org\"" >> $file_name
  for field in "${attributes[@]}";
  do
     REQ_URL="/organizations/${org}/$field/"
     echo "Fetching the count of $field"
     count=($(knife raw -m GET $REQ_URL $FLAG_VALUE | jq length))
     key=$field"_count"
     echo "$key"="$count" >> "$file_name"
   done
done
