package pullandgenerateconfig

const (
	AWS_MODE                          = "AWS_MODE"
	EXISTING_INFRA_MODE               = "EXISTING_INFRA_MODE"
	HA_MODE                           = "HA_MODE"
	AUTOMATE_HA_WORKSPACE_DIR         = "/hab/a2_deploy_workspace"
	AUTOMATE_HA_WORKSPACE_A2HARB_FILE = "/hab/a2_deploy_workspace/a2ha.rb"
	FRONTEND_COMMAND                  = `
	sudo chef-automate config %s /tmp/%s;
	export TIMESTAMP=$(date +'%s');
	sudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;
	sudo chef-automate config show > sudo /etc/chef-automate/config.toml`

	BACKEND_COMMAND = `
	export TIMESTAMP=$(date +"%s");
	echo "yes" | sudo hab config apply automate-ha-%s.default  $(date '+%s') /tmp/%s;
	`

	GET_CONFIG = `
	source <(sudo cat /hab/sup/default/SystemdEnvironmentFile.sh);
	automate-backend-ctl show --svc=automate-ha-%s | tail -n +2
	`

	GET_BACKEND_CONFIG = `
	source <(sudo cat /hab/sup/default/SystemdEnvironmentFile.sh);
	automate-backend-ctl show --svc=automate-ha-%s | tail -n +2 %s
	`

	GET_FRONTEND_CONFIG = `echo "y" | sudo chef-automate config show %s`

	PRE_FLIGHT_CHECK = `cd /tmp;
	chmod +x chef-automate;
	sudo ./chef-automate preflight-check --airgap`

	GET_APPLIED_CONFIG = `
	source <(sudo cat /hab/sup/default/SystemdEnvironmentFile.sh);
	automate-backend-ctl applied --svc=automate-ha-%s
	`

	SUDO_PASSWORD_CMD = `echo "%s" | sudo -S bash -c "`

	STOP_FE_SERVICES_CMD = `sudo systemctl stop chef-automate`
	STOP_BE_SERVICES_CMD = `sudo systemctl stop hab-sup`

	EXCLUDE_OPENSEARCH_NODE_REQUEST = `
	curl --location --request PUT 'http://localhost:10144/_cluster/settings' \
	--header 'Content-Type: application/json' \
	--data-raw '{
	  "persistent" :{
		  "cluster.routing.allocation.exclude._ip" : "%s"
	   }
	}'
	`

	GET_OPENSEARCH_CLUSTER_SETTINGS = `curl --location --request GET 'http://localhost:10144/_cluster/settings'`

	dateFormat       = "%Y%m%d%H%M%S"
	postgresql       = "postgresql"
	opensearch_const = "opensearch"
)
