export endpoint="$1"
export token=`cat token`
export chef_ip="$3"

json_data=$( echo "{ \"id\": \"aasasafaiza43\", \"name\": \"aasasafaiza43\", \"fqdn\": \"\", \"ip_address\": \"$chef_ip\" }")

echo $json_data
curl -k -H "api-token:$token" https://$endpoint/api/v0/infra/servers -d "$json_data"
