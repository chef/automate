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
	PGGETVERSIONCURLCMD = "PGPASSWORD=%s  hab pkg exec core/postgresql13  psql -U %s -h localhost -p 10145 -d postgres --dbname postgres -tAc 'SELECT version()'"
	HABSVCSTATUS        = "echo yes |sudo hab svc status"
	CONFIGSHOW          = "sudo chef-automate config show"
	VERSIONREGEX        = `(\d+\.\d+\.\d+)`
	PGVERSIONREGEX      = `PostgreSQL (\d+\.\d+)`
	OSVERSIONREGEX      = `"number"\s*:\s*"([^"]+)"`
)
