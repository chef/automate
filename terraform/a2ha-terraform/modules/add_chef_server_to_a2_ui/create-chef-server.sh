#!/bin/bash

function add_chef_server(){
export endpoint="$1"
export token=`cat /var/tmp/token`
export chef_ip="$2"
export index="$3"

json_data=$( echo "{ \"id\": \"chef-server.$index\", \"name\": \"chef-server.$index\", \"fqdn\": \"\", \"ip_address\": \"$chef_ip\" }")

curl -k -H "api-token:$token" https://$endpoint/api/v0/infra/servers -d "$json_data"
}

if  grep -q "$2" "/var/tmp/chef_server_list" 
then
   echo "Chef server "$2" is already added to chef_automate UI"
else
   add_chef_server $1 $2 $3
   echo "This chef-server $2 is added to UI" >> chef_server_list
fi
