package config

type HaDeployConfig struct {
	Architecture  *Architecture          `toml:"architecture,omitempty"`
	ObjectStorage *ObjectStorage         `toml:"object_storage,omitempty"`
	Automate      *AutomateSettings      `toml:"automate,omitempty"`
	ChefServer    *ServerConfigSettings  `toml:"chef_server,omitempty"`
	Opensearch    *ServerConfigSettings  `toml:"opensearch,omitempty"`
	Postgresql    *ServerConfigSettings  `toml:"postgresql,omitempty"`
	ExistingInfra *ExistingInfraSettings `toml:"existing_infra,omitempty"`
	External      *ExternalSettings      `toml:"external,omitempty"`
}

type Architecture struct {
	ExistingInfra *ExistingInfraArch `toml:"existing_infra,omitempty"`
}

type ExistingInfraArch struct {
	SSHUser          string `toml:"ssh_user,omitempty"`
	SSHGroupName     string `toml:"ssh_group_name,omitempty"`
	SSHKeyFile       string `toml:"ssh_key_file,omitempty"`
	SSHPort          string `toml:"ssh_port,omitempty"`
	SecretsKeyFile   string `toml:"secrets_key_file,omitempty"`
	SecretsStoreFile string `toml:"secrets_store_file,omitempty"`
	Architecture     string `toml:"architecture,omitempty"`
	WorkspacePath    string `toml:"workspace_path,omitempty"`
	BackupMount      string `toml:"backup_mount,omitempty"`
	BackupConfig     string `toml:"backup_config,omitempty"`
}

type ObjectStorage struct {
	Config *ConfigObjectStorage `toml:"config,omitempty"`
}

type ConfigObjectStorage struct {
	BucketName string `toml:"bucket_name,omitempty"`
	AccessKey  string `toml:"access_key,omitempty"`
	SecretKey  string `toml:"secret_key,omitempty"`
	Endpoint   string `toml:"endpoint,omitempty"`
	Region     string `toml:"region,omitempty"`
}

type AutomateSettings struct {
	Config *ConfigAutomateSettings `toml:"config,omitempty"`
}

type ConfigAutomateSettings struct {
	AdminPassword string `toml:"admin_password,omitempty"`
	Fqdn          string `toml:"fqdn,omitempty"`
	ConfigFile    string `toml:"config_file,omitempty"`
	ConfigSettings
}

type ServerConfigSettings struct {
	Config *ConfigSettings `toml:"config,omitempty"`
}

type ConfigSettings struct {
	InstanceCount     string `toml:"instance_count,omitempty"`
	EnableCustomCerts bool   `toml:"enable_custom_certs,omitempty"`
}

type ExistingInfraSettings struct {
	Config *ConfigExistingInfraSettings `toml:"config,omitempty"`
}

type ConfigExistingInfraSettings struct {
	AutomatePrivateIps   []string `toml:"automate_private_ips,omitempty"`
	ChefServerPrivateIps []string `toml:"chef_server_private_ips,omitempty"`
	OpensearchPrivateIps []string `toml:"opensearch_private_ips,omitempty"`
	PostgresqlPrivateIps []string `toml:"postgresql_private_ips,omitempty"`
}

type ExternalSettings struct {
	Database *ExternalDBSettings `toml:"database,omitempty"`
}

type ExternalDBSettings struct {
	Type       string              `toml:"type,omitempty"`
	PostgreSQL *ExternalPgSettings `toml:"postgre_sql,omitempty"`
	OpenSearch *ExternalOsSettings `toml:"open_search,omitempty"`
}

type ExternalPgSettings struct {
	InstanceURL        string `toml:"instance_url,omitempty"`
	SuperuserUsername  string `toml:"superuser_username,omitempty"`
	SuperuserPassword  string `toml:"superuser_password,omitempty"`
	DbuserUsername     string `toml:"dbuser_username,omitempty"`
	DbuserPassword     string `toml:"dbuser_password,omitempty"`
	PostgresqlRootCert string `toml:"postgresql_root_cert,omitempty"`
}

type ExternalOsSettings struct {
	OpensearchDomainName   string                 `toml:"opensearch_domain_name,omitempty"`
	OpensearchDomainURL    string                 `toml:"opensearch_domain_url,omitempty"`
	OpensearchUsername     string                 `toml:"opensearch_username,omitempty"`
	OpensearchUserPassword string                 `toml:"opensearch_user_password,omitempty"`
	OpensearchRootCert     string                 `toml:"opensearch_root_cert,omitempty"`
	Aws                    *AwsExternalOsSettings `toml:"aws,omitempty"`
}

type AwsExternalOsSettings struct {
	AwsOsSnapshotRoleArn          string `toml:"aws_os_snapshot_role_arn,omitempty"`
	OsSnapshotUserAccessKeyID     string `toml:"os_snapshot_user_access_key_id,omitempty"`
	OsSnapshotUserAccessKeySecret string `toml:"os_snapshot_user_access_key_secret,omitempty"`
}
