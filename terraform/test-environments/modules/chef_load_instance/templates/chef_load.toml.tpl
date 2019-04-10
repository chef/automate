log_file = "/var/log/chef-load/chef-load.log"

ohai_json_file = "${ohai_json_path}"
converge_status_json_file = "${converge_status_json_path}"
compliance_status_json_file = "${compliance_status_json_path}"

# chef-load will evenly distribute the number of nodes across the desired interval (minutes)
# Examples:
#   30 nodes / 30 minute interval =  1 chef-client run per minute
# 1800 nodes / 30 minute interval = 60 chef-client runs per minute
num_nodes = ${num_nodes}
num_actions = ${num_actions}
interval = ${interval}

node_name_prefix = "${node_name_prefix}"

# chef_environment = "_default"

data_collector_token = "${automate_server_token}"

${ "${enable_chef_server_load}" == "true" ?
<<EOF
client_name = "${chef_server_client_name}"
client_key = "/opt/chef-server-client-key.txt"
run_list = [${run_list}]
api_get_requests = [${api_get_requests}]

[chef_server]
host = "${chef_server_fqdn}"
organization = "${chef_server_org}"
EOF
: "" }

[data_collector]
host = "${automate_server_fqdn}"
