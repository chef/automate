#!/bin/bash

function create_token() {
  timestamp=$(date +%s)
  sudo chef-automate iam token create admin.$timestamp --admin > /var/tmp/token
  sudo touch /var/tmp/token-generated.Done
}

function add_chef_server(){
export endpoint="$1"
export token=`cat /var/tmp/token`
export chef_fqdn="$2"

json_data=$( echo "{ \"id\": \"chef-server\", \"name\": \"chef-server\", \"fqdn\": \"$chef_fqdn\", \"ip_address\": \"\" }")

curl -k -H "api-token:$token" https://$endpoint/api/v0/infra/servers -d "$json_data"
}

if [ -e /var/tmp/token-generated.Done ]
then
   echo "This is an upgrade scenario, where token is already generated and chef-server is added to Automate UI"
else
   create_token 
   add_chef_server $1 $2
fi