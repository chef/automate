package config

import (
	"encoding/json"
	"fmt"

	"github.com/chef/automate/lib/io/fileutils"
	ptoml "github.com/pelletier/go-toml"
)

type HaDeployConfig struct {
	Architecture  *Architecture          `toml:"architecture,omitempty"`
	ObjectStorage *ObjectStorage         `toml:"object_storage,omitempty"`
	Automate      *AutomateSettings      `toml:"automate,omitempty"`
	ChefServer    *ChefServerSettings    `toml:"chef_server,omitempty"`
	Opensearch    *OpensearchSettings    `toml:"opensearch,omitempty"`
	Postgresql    *PostgresqlSettings    `toml:"postgresql,omitempty"`
	ExistingInfra *ExistingInfraSettings `toml:"existing_infra,omitempty"`
	Aws           *AwsSettings           `toml:"aws,omitempty"`
	External      *ExternalSettings      `toml:"external,omitempty"`
}

type GcpServiceAccount struct {
	Type                    string `json:"type"`
	ProjectID               string `json:"project_id"`
	PrivateKeyID            string `json:"private_key_id"`
	PrivateKey              string `json:"private_key"`
	ClientEmail             string `json:"client_email"`
	ClientID                string `json:"client_id"`
	AuthURI                 string `json:"auth_uri"`
	TokenURI                string `json:"token_uri"`
	AuthProviderX509CertURL string `json:"auth_provider_x509_cert_url"`
	ClientX509CertURL       string `json:"client_x509_cert_url"`
	UniverseDomain          string `json:"universe_domain"`
}

type Architecture struct {
	ExistingInfra *ConfigInitials `toml:"existing_infra,omitempty"`
	Aws           *ConfigInitials `toml:"aws,omitempty"`
}

type ConfigInitials struct {
	SSHUser                     string `toml:"ssh_user,omitempty"`
	SSHGroupName                string `toml:"ssh_group_name,omitempty"`
	SSHKeyFile                  string `toml:"ssh_key_file,omitempty"`
	SSHPort                     string `toml:"ssh_port,omitempty"`
	SecretsKeyFile              string `toml:"secrets_key_file,omitempty"`
	SecretsStoreFile            string `toml:"secrets_store_file,omitempty"`
	SudoPassword                string `toml:"sudo_password,omitempty"`
	LoggingMonitoringManagement string `toml:"logging_monitoring_management,omitempty"`
	Architecture                string `toml:"architecture,omitempty"`
	WorkspacePath               string `toml:"workspace_path,omitempty"`
	BackupMount                 string `toml:"backup_mount,omitempty"`
	BackupConfig                string `toml:"backup_config,omitempty"`
	S3BucketName                string `toml:"s3_bucketName,omitempty"`
	HabitatUIDGid               string `toml:"habitat_uid_gid,omitempty"`
	// Elk configs not being used, it's not added
}

type ObjectStorage struct {
	Config *ConfigObjectStorage `toml:"config,omitempty"`
}

type CertByIP struct {
	IP         string `toml:"ip,omitempty"`
	PrivateKey string `toml:"private_key,omitempty"`
	PublicKey  string `toml:"public_key,omitempty"`
	NodesDn    string `toml:"nodes_dn,omitempty"`
}

type ConfigObjectStorage struct {
	Location          string             `toml:"location,omitempty"`
	BucketName        string             `toml:"bucket_name,omitempty"`
	AccessKey         string             `toml:"access_key,omitempty"`
	SecretKey         string             `toml:"secret_key,omitempty"`
	Endpoint          string             `toml:"endpoint,omitempty"`
	Region            string             `toml:"region,omitempty"`
	GcpServiceFile    string             `toml:"gcp_service_file,omitempty"`
	GcpServiceAccount *GcpServiceAccount `toml:"gcp_service_account,omitempty"`
}

type AutomateSettings struct {
	Config *ConfigAutomateSettings `toml:"config,omitempty"`
}
type ConfigAutomateSettings struct {
	AdminPassword     string      `toml:"admin_password,omitempty"`
	Fqdn              string      `toml:"fqdn,omitempty"`
	ConfigFile        string      `toml:"config_file,omitempty"`
	TeamsPort         string      `toml:"teams_port,omitempty"`
	FqdnRootCA        string      `toml:"root_ca,omitempty"`
	InstanceCount     string      `toml:"instance_count,omitempty"`
	EnableCustomCerts bool        `toml:"enable_custom_certs,omitempty"`
	PrivateKey        string      `toml:"private_key,omitempty"`
	PublicKey         string      `toml:"public_key,omitempty"`
	CertsByIP         *[]CertByIP `toml:"certs_by_ip,omitempty"`
}

type ChefServerSettings struct {
	Config *ConfigChefServerSettings `toml:"config,omitempty"`
}

type PostgresqlSettings struct {
	Config *ConfigSettings `toml:"config,omitempty"`
}

type OpensearchSettings struct {
	Config *ConfigOpensearchSettings `toml:"config,omitempty"`
}

type ConfigChefServerSettings struct {
	ChefServerFqdn    string      `toml:"fqdn,omitempty"`
	FqdnRootCA        string      `toml:"lb_root_ca,omitempty"`
	InstanceCount     string      `toml:"instance_count,omitempty"`
	EnableCustomCerts bool        `toml:"enable_custom_certs,omitempty"`
	PrivateKey        string      `toml:"private_key,omitempty"`
	PublicKey         string      `toml:"public_key,omitempty"`
	CertsByIP         *[]CertByIP `toml:"certs_by_ip,omitempty"`
}
type ConfigOpensearchSettings struct {
	AdminCert         string      `toml:"admin_cert,omitempty"`
	AdminKey          string      `toml:"admin_key,omitempty"`
	AdminDn           string      `toml:"admin_dn,omitempty"`
	NodesDn           string      `toml:"nodes_dn,omitempty"`
	RootCA            string      `toml:"root_ca,omitempty"`
	InstanceCount     string      `toml:"instance_count,omitempty"`
	EnableCustomCerts bool        `toml:"enable_custom_certs,omitempty"`
	PrivateKey        string      `toml:"private_key,omitempty"`
	PublicKey         string      `toml:"public_key,omitempty"`
	CertsByIP         *[]CertByIP `toml:"certs_by_ip,omitempty"`
}

type ConfigSettings struct {
	RootCA            string      `toml:"root_ca,omitempty"`
	InstanceCount     string      `toml:"instance_count,omitempty"`
	EnableCustomCerts bool        `toml:"enable_custom_certs,omitempty"`
	PrivateKey        string      `toml:"private_key,omitempty"`
	PublicKey         string      `toml:"public_key,omitempty"`
	CertsByIP         *[]CertByIP `toml:"certs_by_ip,omitempty"`
}

type ExistingInfraSettings struct {
	Config *ConfigExistingInfraSettings `toml:"config,omitempty"`
}

type AwsSettings struct {
	Config *ConfigAwsSettings `toml:"config,omitempty"`
}

type ConfigExistingInfraSettings struct {
	AutomatePrivateIps   []string `toml:"automate_private_ips,omitempty"`
	ChefServerPrivateIps []string `toml:"chef_server_private_ips,omitempty"`
	OpensearchPrivateIps []string `toml:"opensearch_private_ips,omitempty"`
	PostgresqlPrivateIps []string `toml:"postgresql_private_ips,omitempty"`
}

type ConfigAwsSettings struct {
	Profile                       string   `toml:"profile,omitempty"`
	Region                        string   `toml:"region,omitempty"`
	AwsVpcID                      string   `toml:"aws_vpc_id,omitempty"`
	AwsCidrBlockAddr              string   `toml:"aws_cidr_block_addr,omitempty"`
	PrivateCustomSubnets          []string `toml:"private_custom_subnets,omitempty"`
	PublicCustomSubnets           []string `toml:"public_custom_subnets,omitempty"`
	SSHKeyPairName                string   `toml:"ssh_key_pair_name,omitempty"`
	SetupManagedServices          bool     `toml:"setup_managed_services,omitempty"`
	ManagedOpensearchDomainName   string   `toml:"managed_opensearch_domain_name,omitempty"`
	ManagedOpensearchDomainURL    string   `toml:"managed_opensearch_domain_url,omitempty"`
	ManagedOpensearchUsername     string   `toml:"managed_opensearch_username,omitempty"`
	ManagedOpensearchUserPassword string   `toml:"managed_opensearch_user_password,omitempty"`
	ManagedOpensearchCertificate  string   `toml:"managed_opensearch_certificate,omitempty"`
	AwsOsSnapshotRoleArn          string   `toml:"aws_os_snapshot_role_arn"`
	OsSnapshotUserAccessKeyID     string   `toml:"os_snapshot_user_access_key_id"`
	OsSnapshotUserAccessKeySecret string   `toml:"os_snapshot_user_access_key_secret"`
	ManagedRdsInstanceURL         string   `toml:"managed_rds_instance_url,omitempty"`
	ManagedRdsSuperuserUsername   string   `toml:"managed_rds_superuser_username,omitempty"`
	ManagedRdsSuperuserPassword   string   `toml:"managed_rds_superuser_password,omitempty"`
	ManagedRdsDbuserUsername      string   `toml:"managed_rds_dbuser_username,omitempty"`
	ManagedRdsDbuserPassword      string   `toml:"managed_rds_dbuser_password,omitempty"`
	ManagedRdsCertificate         string   `toml:"managed_rds_certificate,omitempty"`
	AmiID                         string   `toml:"ami_id,omitempty"`
	DeleteOnTermination           bool     `toml:"delete_on_termination,omitempty"`
	AutomateServerInstanceType    string   `toml:"automate_server_instance_type,omitempty"`
	ChefServerInstanceType        string   `toml:"chef_server_instance_type,omitempty"`
	OpensearchServerInstanceType  string   `toml:"opensearch_server_instance_type,omitempty"`
	PostgresqlServerInstanceType  string   `toml:"postgresql_server_instance_type,omitempty"`
	AutomateLbCertificateArn      string   `toml:"automate_lb_certificate_arn,omitempty"`
	ChefServerLbCertificateArn    string   `toml:"chef_server_lb_certificate_arn,omitempty"`
	ChefEbsVolumeIops             string   `toml:"chef_ebs_volume_iops,omitempty"`
	ChefEbsVolumeSize             string   `toml:"chef_ebs_volume_size,omitempty"`
	ChefEbsVolumeType             string   `toml:"chef_ebs_volume_type,omitempty"`
	OpensearchEbsVolumeIops       string   `toml:"opensearch_ebs_volume_iops,omitempty"`
	OpensearchEbsVolumeSize       string   `toml:"opensearch_ebs_volume_size,omitempty"`
	OpensearchEbsVolumeType       string   `toml:"opensearch_ebs_volume_type,omitempty"`
	PostgresqlEbsVolumeIops       string   `toml:"postgresql_ebs_volume_iops,omitempty"`
	PostgresqlEbsVolumeSize       string   `toml:"postgresql_ebs_volume_size,omitempty"`
	PostgresqlEbsVolumeType       string   `toml:"postgresql_ebs_volume_type,omitempty"`
	AutomateEbsVolumeIops         string   `toml:"automate_ebs_volume_iops,omitempty"`
	AutomateEbsVolumeSize         string   `toml:"automate_ebs_volume_size,omitempty"`
	AutomateEbsVolumeType         string   `toml:"automate_ebs_volume_type,omitempty"`
	AmiFilterName                 string   `toml:"ami_filter_name,omitempty"`
	AmiFilterVirtType             string   `toml:"ami_filter_virt_type,omitempty"`
	AmiFilterOwner                string   `toml:"ami_filter_owner,omitempty"`
	LbAccessLogs                  string   `toml:"lb_access_logs,omitempty"`
	XContact                      string   `toml:"X-Contact,omitempty"`
	XDept                         string   `toml:"X-Dept,omitempty"`
	XProject                      string   `toml:"X-Project,omitempty"`
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
	InstanceURL           string `toml:"instance_url,omitempty"`
	SuperuserUsername     string `toml:"superuser_username,omitempty"`
	SuperuserPassword     string `toml:"superuser_password,omitempty"`
	DbuserUsername        string `toml:"dbuser_username,omitempty"`
	DbuserPassword        string `toml:"dbuser_password,omitempty"`
	PostgresqlRootCert    string `toml:"postgresql_root_cert,omitempty"`
	PostgreSQLCertificate string `toml:"postgresql_certificate,omitempty"`
}

type ExternalOsSettings struct {
	OpensearchDomainName   string                 `toml:"opensearch_domain_name,omitempty"`
	OpensearchDomainURL    string                 `toml:"opensearch_domain_url,omitempty"`
	OpensearchUsername     string                 `toml:"opensearch_username,omitempty"`
	OpensearchUserPassword string                 `toml:"opensearch_user_password,omitempty"`
	OpensearchCertificate  string                 `toml:"opensearch_certificate,omitempty"`
	OpensearchRootCert     string                 `toml:"opensearch_root_cert,omitempty"`
	Aws                    *AwsExternalOsSettings `toml:"aws,omitempty"`
}

type AwsExternalOsSettings struct {
	AwsOsSnapshotRoleArn          string `toml:"aws_os_snapshot_role_arn,omitempty"`
	OsSnapshotUserAccessKeyID     string `toml:"os_snapshot_user_access_key_id,omitempty"`
	OsSnapshotUserAccessKeySecret string `toml:"os_snapshot_user_access_key_secret,omitempty"`
}

func NewHaDeployConfig() *HaDeployConfig {
	return &HaDeployConfig{}
}

func (c *HaDeployConfig) Parse(configFile string) error {
	fileUtils := &fileutils.FileSystemUtils{}
	templateBytes, err := fileUtils.ReadFile(configFile)
	if err != nil {
		return fmt.Errorf("error reading config TOML file: %w", err)
	}
	err = ptoml.Unmarshal(templateBytes, c) // Pass pointer to c
	if err != nil {
		return fmt.Errorf("error unmarshalling config TOML file: %w", err)
	}
	gcpServiceAccount := GcpServiceAccount{}
	if c.GetConfigInitials() != nil && c.GetConfigInitials().BackupConfig == "object_storage" {
		objectStorageConfig := c.GetObjectStorageConfig()
		if objectStorageConfig.Location == "gcs" {
			filepath, err := fileUtils.ReadFile(objectStorageConfig.GcpServiceFile)
			if err != nil {
				return fmt.Errorf("error reading Json file: %w", err)
			}
			err = json.Unmarshal(filepath, &gcpServiceAccount)
			if err != nil {
				return fmt.Errorf("error unmarshalling Json file: %w", err)
			}
			c.ObjectStorage.Config.GcpServiceAccount = &gcpServiceAccount
		}
	}
	return nil
}

func (c *HaDeployConfig) InitArchitecture() *Architecture {
	if c.Architecture == nil {
		c.Architecture = &Architecture{}
	}
	return c.Architecture
}

func (c *Architecture) InitExistingInfra() *ConfigInitials {
	if c.ExistingInfra == nil {
		c.ExistingInfra = &ConfigInitials{}
	}
	return c.ExistingInfra
}

func (c *Architecture) InitAws() *ConfigInitials {
	if c.Aws == nil {
		c.Aws = &ConfigInitials{}
	}
	return c.Aws
}

func (c *HaDeployConfig) InitAutomate() *AutomateSettings {
	if c.Automate == nil {
		c.Automate = &AutomateSettings{}
	}
	return c.Automate
}

func (c *AutomateSettings) InitConfig() *ConfigAutomateSettings {
	if c.Config == nil {
		c.Config = &ConfigAutomateSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitChefServer() *ChefServerSettings {
	if c.ChefServer == nil {
		c.ChefServer = &ChefServerSettings{}
	}
	return c.ChefServer
}

func (c *ChefServerSettings) InitConfig() *ConfigChefServerSettings {
	if c.Config == nil {
		c.Config = &ConfigChefServerSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitOpenSearch() *OpensearchSettings {
	if c.Opensearch == nil {
		c.Opensearch = &OpensearchSettings{}
	}
	return c.Opensearch
}

func (c *OpensearchSettings) InitConfig() *ConfigOpensearchSettings {
	if c.Config == nil {
		c.Config = &ConfigOpensearchSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitPostgresql() *PostgresqlSettings {
	if c.Postgresql == nil {
		c.Postgresql = &PostgresqlSettings{}
	}
	return c.Postgresql
}

func (c *PostgresqlSettings) InitConfig() *ConfigSettings {
	if c.Config == nil {
		c.Config = &ConfigSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitExistingInfra() *ExistingInfraSettings {
	if c.ExistingInfra == nil {
		c.ExistingInfra = &ExistingInfraSettings{}
	}
	return c.ExistingInfra
}

func (c *ExistingInfraSettings) InitConfig() *ConfigExistingInfraSettings {
	if c.Config == nil {
		c.Config = &ConfigExistingInfraSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitExternal() *ExternalSettings {
	if c.External == nil {
		c.External = &ExternalSettings{}
	}
	return c.External
}

func (c *ExternalSettings) InitDatabase() *ExternalDBSettings {
	if c.Database == nil {
		c.Database = &ExternalDBSettings{}
	}
	return c.Database
}

func (c *ExternalDBSettings) InitPostgresql() *ExternalPgSettings {
	if c.PostgreSQL == nil {
		c.PostgreSQL = &ExternalPgSettings{}
	}
	return c.PostgreSQL
}

func (c *ExternalDBSettings) InitOpenSearch() *ExternalOsSettings {
	if c.OpenSearch == nil {
		c.OpenSearch = &ExternalOsSettings{}
	}
	return c.OpenSearch
}

func (c *ExternalOsSettings) InitOpenSearchAws() *AwsExternalOsSettings {
	if c.Aws == nil {
		c.Aws = &AwsExternalOsSettings{}
	}
	return c.Aws
}

func (c *ConfigAutomateSettings) InitCertsByIP() *[]CertByIP {
	if c.CertsByIP == nil {
		c.CertsByIP = &[]CertByIP{}
	}
	return c.CertsByIP
}

func (c *ConfigSettings) InitCertsByIP() *[]CertByIP {
	if c.CertsByIP == nil {
		c.CertsByIP = &[]CertByIP{}
	}
	return c.CertsByIP
}

func (c *ConfigChefServerSettings) InitCertsByIP() *[]CertByIP {
	if c.CertsByIP == nil {
		c.CertsByIP = &[]CertByIP{}
	}
	return c.CertsByIP
}

func (c *ConfigOpensearchSettings) InitCertsByIP() *[]CertByIP {
	if c.CertsByIP == nil {
		c.CertsByIP = &[]CertByIP{}
	}
	return c.CertsByIP
}

func (c *HaDeployConfig) InitAws() *AwsSettings {
	if c.Aws == nil {
		c.Aws = &AwsSettings{}
	}
	return c.Aws
}

func (c *AwsSettings) InitConfigAwsSettings() *ConfigAwsSettings {
	if c.Config == nil {
		c.Config = &ConfigAwsSettings{}
	}
	return c.Config
}

func (c *HaDeployConfig) InitObjectStorage() *ObjectStorage {
	if c.ObjectStorage == nil {
		c.ObjectStorage = &ObjectStorage{}
	}
	return c.ObjectStorage
}

func (c *ObjectStorage) InitConfig() *ConfigObjectStorage {
	if c.Config == nil {
		c.Config = &ConfigObjectStorage{}
	}
	return c.Config
}
