package main

const (
	FRONTEND            = "frontend"
	AUTOMATE            = "automate"
	CHEF_SERVER         = "chef_server"
	POSTGRESQL          = "postgresql"
	OPENSEARCH          = "opensearch"
	SET                 = "set"
	PATCH               = "patch"
	SUDO_PASSWORD       = "sudo_password"
	IPV4REGEX           = `^(((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4})`
	A2VERSIONCMD        = "sudo chef-automate version"
	A2VERSIONVERBOSE    = "sudo chef-automate version -v "
	CSVERSIONCMD        = "sudo chef-server-ctl version"
	OSGETINFOCURLCMD    = "curl -XGET http://localhost:10144"
	PGGETVERSIONCURLCMD = "PGPASSWORD=%s  hab pkg exec %s  psql -U %s -h localhost -p 10145 -d postgres --dbname postgres -tAc 'SELECT version()'"
	HABSVCSTATUS        = "echo yes |sudo hab svc status"
	CONFIGSHOW          = "sudo chef-automate config show"
	VERSIONREGEX        = `(\d+\.\d+\.\d+)`
	PGVERSIONREGEX      = `PostgreSQL (\d+\.\d+)`
	OSVERSIONREGEX      = `"number"\s*:\s*"([^"]+)"`
	PGCOREPKG           = "core/postgresql13 "
	AUTOMATE_NAME       = "Automate"
	BASTION_NAME        = "Bastion"
	CHEF_SERVER_NAME    = "Chef Server"
	OPENSEARCH_NAME     = "Opensearch"
	POSTGRESQL_NAME     = "Postgresql"
	AUTOMATE_TOML       = "automate.config.toml"
	CHEF_SERVER_TOML    = "chef_server.config.toml"
	POSTGRESQL_TOML     = "postgresql.config.toml"
	OPENSEARCH_TOML     = "opensearch.config.toml"
	OCID_SHOW_APP       = "oc-id-show-app"
	HOME_DIR            = "/home"
	TMP_DIR             = "/tmp"
)
