#/bin/bash
export endpoint="$1"
export token=`cat /hab/a2_deploy_workspace/terraform/token`
export chef_ip="$2"

json_data=$( echo "{ \"id\": \"chef-server.$3\", \"name\": \"chef-server.$3\", \"fqdn\": \"\", \"ip_address\": \"$chef_ip\" }")

curl -k -H "api-token:$token" https://$endpoint/api/v0/infra/servers -d "$json_data"
