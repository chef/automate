#!/bin/bash
#
# Authors: Salim Afiune <afiune@chef.io>
# Modified by: Josh Hudson <jhudson@chef.io>
#
function usage() {
  echo "Usage: This script is meant to be invoked with an automate-credentials.toml file"
  echo "       to provide the data-collector token to the chef-load server."
  echo ""
  echo "Example: Generate a token with the default admin user."
  echo "------------------------------------------------------"
  echo "./scripts/generate-data-collector.sh /home/centos/automate-credentials.toml"
  echo ""
  echo "Output:"
  echo "{"
  echo ' "token": "gyO5UK59gYkmdRGiCkT7mLXPONk="'
  echo "}"
}

if [ ! -f "$1" ]; then
  usage && exit 1
else
  automate_url=$(awk -F '"' 'NR==1 {print $2}' automate-credentials.toml)
  user=$(awk -F '"' 'NR==2 {print $2}' automate-credentials.toml)
  password=$(awk -F '"' 'NR==3 {print $2}' automate-credentials.toml)
fi

[ "$automate_url" == null ] && echo "Can't parse automate_url" && exit 1
[ "$user" == null ] && echo "Can't parse user" && exit 1
[ "$password" == null ] && echo "Can't parse password" && exit 1

# Uses same method to get id_token as
# https://github.com/chef/a2/blob/0e87b81408a64f938749fa213d9773e4d59af0e3/inspec/a2-resource-pack/libraries/automate_api_request.rb#L58-L93
req_loc=$(curl -k "${automate_url}/dex/auth?client_id=automate-api&scope=openid+profile+email+offline_access+groups+federated:id&response_type=code+id_token&state=inspec&nonce=yeahnotreally&redirect_uri=urn:ietf:wg:oauth:2.0:oob" 2>/dev/null | cut -d\" -f2)

# Display form (workaround dex issue: https://github.com/coreos/dex/pull/1144)
curl -k --silent --output /dev/null "${automate_url}${req_loc%$'\r'}"
approval_loc=$(curl -k -D - "${automate_url}${req_loc%$'\r'}" -d login="$user" -d password="$password" 2>/dev/null | grep -w Location | cut -d' ' -f2)
code=$(curl -k -D - "${automate_url}${approval_loc%$'\r'}" 2>/dev/null | grep -w value | cut -d= -f4 | cut -d '"' -f2)

# Authorization:Basic YXV0b21hdGUtYXBpOg== is base64 encoded username and password
# of 'automate-api' and '' respectively which we've set up in the hab dex config.
id_token=$(curl -k "${automate_url}/dex/token" -d "grant_type=authorization_code&redirect_uri=urn:ietf:wg:oauth:2.0:oob&code=${code}" -H "Content-Type:application/x-www-form-urlencoded" -H "Authorization:Basic YXV0b21hdGUtYXBpOg==" 2>/dev/null | jq '.id_token' | cut -d '"' -f2)

token_json_response=$(curl -k -H "Authorization: Bearer ${id_token}" "${automate_url}/api/v0/auth/tokens" -d '{"description": "data-collector-token", "active": true}' 2>/dev/null)
token=$(echo "$token_json_response" | jq '.value' | sed 's/"//g')


[ "$token" == null ] && echo "{\"error\":\"unable to generate token\"}" && exit 1

# TODO: get this merged in the chef-load plan
mkdir -p /var/log/chef-load
chown hab:root /var/log/chef-load
mkdir -p /hab/svc/chef-load
chown hab:root /hab/svc/chef-load
mkdir -p /hab/user/chef-load/config/
echo "Writing chef-load user.toml"
echo """data_collector_token = \"$token\" data_collector.host = \"$automate_url\"""" > /hab/user/chef-load/config/user.toml
