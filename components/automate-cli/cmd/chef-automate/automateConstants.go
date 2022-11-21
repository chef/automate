package main

const (
	AWS_MODE            = "AWS_MODE"
	EXISTING_INFRA_MODE = "EXISTING_INFRA_MODE"
	AUTOMATE            = "AUTOMATE"
	HA_MODE             = "HA_MODE"
)

const AUTOMATE_HA_RUN_LOG_DIR = "/hab/a2_deploy_workspace/logs"
const AUTOMATE_HA_WORKSPACE_DIR = "/hab/a2_deploy_workspace"
const AUTOMATE_HA_WORKSPACE_CONFIG_FILE = "/hab/a2_deploy_workspace/config.toml"
const AUTOMATE_HA_AUTOMATE_CONFIG_FILE = "/hab/a2_deploy_workspace/configs/automate.toml"
const AUTOMATE_HA_INVALID_BASTION = "Invalid bastion, to run this command use automate bastion"
const AIRGAP_HA_TRANS_DIR_PATH = "/hab/a2_deploy_workspace/terraform/transfer_files/"
const AUTOMATE_HA_TERRAFORM_DIR = "/hab/a2_deploy_workspace/terraform/"
const AUTOMATE_HA_FILE_PERMISSION_0755 = 0755
const AUTOMATE_HA_FILE_PERMISSION_0644 = 0644

const frontendAutotfvarsTemplate = `
frontend_aib_dest_file = "/var/tmp/{{ .bundleName }}"
frontend_aib_local_file = "{{ .bundleName }}"
		`

const backendAutotfvarsTemplate = `
backend_aib_dest_file = "/var/tmp/{{ .backendBundleFile }}"
backend_aib_local_file = "{{ .backendBundleFile }}"
`

const AUTOMATE_HA_PKG_PG_LDR_CHK = "automate-ha-pgleaderchk"
const AUTOMATE_HA_PKG_PG = "automate-ha-postgresql"
const AUTOMATE_HA_PKG_HA_PROXY = "automate-ha-haproxy"
const AUTOMATE_HA_OS = "automate-ha-opensearch"
const AUTOMATE_HA_ES_CAR = "automate-ha-elasticsidecar"

const ORIGIN_PATTERN = `[a-zA-z0-9]*`
const PACKAGE_NAME_PATTERN = `-[a-zA-Z0-9]*-.*-\d+\.`
const RELEASE_AND_VERSION_PATTERN = `.*-(\d+\.\d+\.*\d*)-(\d{14})-.*\.hart$`

const (
	FRONTEND_COMMANDS = `
	sudo chef-automate config patch /tmp/%s;
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

	GET_FRONTEND_CONFIG = `sudo chef-automate config show`
)
