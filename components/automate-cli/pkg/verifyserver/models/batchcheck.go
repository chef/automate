package models

import (
	"github.com/chef/automate/lib/config"
	"github.com/gofiber/fiber/v2"
)

type BatchCheckRequest struct {
	Checks []string `json:"checks"`
	Config Config   `json:"config"`
}

type Hardware struct {
	AutomateNodeCount        int      `json:"automate_node_count"`
	AutomateNodeIps          []string `json:"automate_node_ips"`
	ChefInfraServerNodeCount int      `json:"chef_infra_server_node_count"`
	ChefInfraServerNodeIps   []string `json:"chef_infra_server_node_ips"`
	PostgresqlNodeCount      int      `json:"postgresql_node_count"`
	PostgresqlNodeIps        []string `json:"postgresql_node_ips"`
	OpenSearchNodeCount      int      `json:"opensearch_node_count"`
	OpenSearchNodeIps        []string `json:"opensearch_node_ips"`
}
type SSHUser struct {
	Username     string `json:"user_name"`
	PrivateKey   string `json:"private_key"`
	SudoPassword string `json:"sudo_password"`
}

type FileSystem struct {
	MountLocation string `json:"mount_location"`
}
type ObjectStorage struct {
	Endpoint   string `json:"endpoint"`
	BucketName string `json:"bucket_name"`
	BasePath   string `json:"base_path"`
	AccessKey  string `json:"access_key"`
	SecretKey  string `json:"secret_key"`
	AWSRegion  string `json:"aws_region"`
}
type Backup struct {
	FileSystem    FileSystem    `json:"file_system"`
	ObjectStorage ObjectStorage `json:"object_storage"`
}

type Certificate struct {
	AutomateFqdn   string     `json:"automate_fqdn"`
	ChefServerFqdn string     `json:"cs_fqdn"`
	RootCert       string     `json:"root_cert"`
	Nodes          []NodeCert `json:"nodes"`
}

type ExternalOS struct {
	OSDomainName   string `json:"opensearch_domain_name"`
	OSDomainURL    string `json:"opensearch_domain_url"`
	OSUsername     string `json:"opensearch_username"`
	OSUserPassword string `json:"opensearch_user_password"`
	OSCert         string `json:"opensearch_cert"`
	OSRoleArn      string `json:"opensearch_role_arn"`
}

type ExternalPG struct {
	PGInstanceURL       string `json:"postgresql_instance_url"`
	PGSuperuserName     string `json:"postgresql_superuser_username"`
	PGSuperuserPassword string `json:"postgresql_superuser_password"`
	PGDbUserName        string `json:"postgresql_dbuser_username"`
	PGDbUserPassword    string `json:"postgresql_dbuser_password"`
	PGRootCert          string `json:"postgresql_root_cert"`
}

type Config struct {
	SSHUser         SSHUser     `json:"ssh_user"`
	Arch            string      `json:"arch"`
	Backup          Backup      `json:"backup"`
	Hardware        Hardware    `json:"hardware"`
	Certificate     Certificate `json:"certificate"`
	ExternalOS      ExternalOS  `json:"external_opensearch"`
	ExternalPG      ExternalPG  `json:"external_postgresql"`
	DeploymentState string      `json:"deployment_state"`
	APIToken        string      `json:"api_token"`
}

func (c *Config) PopulateWith(haConfig *config.HaDeployConfig) error {
	err := haConfig.Verify()
	if err != nil {
		return err
	}

	c.SSHUser.Username = haConfig.GetConfigInitials().SSHUser
	c.SSHUser.PrivateKey = haConfig.GetConfigInitials().SSHKeyFile
	c.SSHUser.SudoPassword = haConfig.GetConfigInitials().SudoPassword
	c.Arch = haConfig.GetConfigInitials().Architecture
	c.Backup.FileSystem.MountLocation = haConfig.GetConfigInitials().BackupMount
	c.Backup.ObjectStorage.BucketName = haConfig.GetObjectStorageConfig().BucketName
	c.Backup.ObjectStorage.AWSRegion = haConfig.GetObjectStorageConfig().Region
	c.Backup.ObjectStorage.AccessKey = haConfig.GetObjectStorageConfig().AccessKey
	c.Backup.ObjectStorage.SecretKey = haConfig.GetObjectStorageConfig().SecretKey
	c.Backup.ObjectStorage.Endpoint = haConfig.GetObjectStorageConfig().Endpoint

	//WIP
	// c.Hardware.AutomateNodeCount = haConfig.GetAutomateNodeCount()
	// c.Hardware.ChefInfraServerNodeCount = haConfig.GetChefServerNodeCount()
	// c.Hardware.PostgresqlNodeCount = haConfig.GetPostgresqlNodeCount()
	// c.Hardware.OpenSearchNodeCount = haConfig.GetOpenSearchNodeCount()
	// c.Hardware.AutomateNodeIps = haConfig.GetAutomateNodeIps()
	// c.Hardware.ChefInfraServerNodeIps = haConfig.GetChefServerNodeIps()
	// c.Hardware.PostgresqlNodeIps = haConfig.GetPostgresqlNodeIps()
	// c.Hardware.OpenSearchNodeIps = haConfig.GetOpenSearchNodeIps()
	// c.Certificate.AutomateFqdn = haConfig.GetAutomateConfig().Fqdn
	// // chef_server_config is not provided in config
	// c.Certificate.ChefServerFqdn = ""
	// // root_ca of which node?
	// c.Certificate.RootCert = haConfig.GetAutomateConfig().RootCA
	// // c.Certificate.Nodes =
	// c.ExternalPG.PGDbUserName = haConfig.GetExternalPgConfig().DbuserUsername
	// c.ExternalPG.PGDbUserPassword = haConfig.GetExternalPgConfig().DbuserPassword
	// c.ExternalPG.PGInstanceURL = haConfig.GetExternalPgConfig().InstanceURL
	// c.ExternalPG.PGRootCert = haConfig.GetExternalPgConfig().PostgresqlRootCert
	// c.ExternalPG.PGSuperuserName = haConfig.GetExternalPgConfig().SuperuserUsername
	// c.ExternalPG.PGSuperuserPassword = haConfig.GetExternalPgConfig().SuperuserPassword
	// c.ExternalOS.OSDomainName = haConfig.GetExternalOsConfig().OpensearchDomainName
	// c.ExternalOS.OSDomainURL = haConfig.GetExternalOsConfig().OpensearchDomainURL
	// c.ExternalOS.OSRoleArn = haConfig.GetExternalOsConfig().Aws.AwsOsSnapshotRoleArn
	// c.ExternalOS.OSUserPassword = haConfig.GetExternalOsConfig().OpensearchUserPassword
	// c.ExternalOS.OSUsername = haConfig.GetExternalOsConfig().OpensearchUsername

	// not available in config
	c.DeploymentState = ""
	c.APIToken = ""

	return nil
}

type BatchCheckResponse struct {
	Status string             `json:"status"`
	Result []BatchCheckResult `json:"result"`
}
type BatchCheckResult struct {
	NodeType string      `json:"node_type"`
	Ip       string      `json:"ip"`
	Tests    []ApiResult `json:"tests"`
}

type CheckTriggerResponse struct {
	Status    string    `json:"status"`
	Result    ApiResult `json:"result"`
	Host      string    `json:"host"`
	NodeType  string    `json:"node_type"`
	CheckType string    `json:"check_type"`
}
type ApiResult struct {
	Passed  bool         `json:"passed"`
	Message string       `json:"msg"`
	Check   string       `json:"check"`
	Checks  []Checks     `json:"checks"`
	Error   *fiber.Error `json:"error,omitempty"`
}

type Checks struct {
	Title         string `json:"title"`
	Passed        bool   `json:"passed"`
	SuccessMsg    string `json:"success_msg"`
	ErrorMsg      string `json:"error_msg"`
	ResolutionMsg string `json:"resolution_msg"`
}

// is this supposed to be cert_by_ip? this struct needs modifiation
type NodeCert struct {
	IP        string `json:"ip"`
	Cert      string `json:"cert"`
	Key       string `json:"key"`
	AdminKey  string `json:"admin_key"`
	AdminCert string `json:"admin_cert"`
}

type HardwareResourceCheckResponse struct {
	Status string                           `json:"status"`
	Result []HardwareResourceCountApiResult `json:"result"`
}

type HardwareResourceCountApiResult struct {
	IP       string   `json:"ip"`
	NodeType string   `json:"node_type"`
	Checks   []Checks `json:"checks"`
}

type CertificateCheckRequest struct {
	RootCertificate  string `json:"root_certificate"`
	PrivateKey       string `json:"private_key"`
	NodeCertificate  string `json:"node_certificate"`
	AdminPrivateKey  string `json:"admin_private_key"`
	AdminCertificate string `json:"admin_certificate"`
}

type SShUserRequest struct {
	IP           string `json:"ip"`
	Username     string `json:"user_name"`
	PrivateKey   string `json:"private_key"`
	SudoPassword string `json:"sudo_password"`
}

type FirewallRequest struct {
	SourceNodeIP               string `json:"source_node_ip"`
	DestinationNodeIP          string `json:"destination_node_ip"`
	DestinationServicePort     string `json:"destination_service_port"`
	DestinationServiceProtocol string `json:"destination_service_protocol"`
	RootCert                   string `json:"root_cert"`
}

type NFSMountCheckResponse struct {
	Status string             `json:"status"`
	Result []NFSMountResponse `json:"result"`
}

type FqdnRequest struct {
	Fqdn              string   `json:"fqdn"`
	RootCert          string   `json:"root_cert"`
	IsAfterDeployment bool     `json:"is_after_deployment"`
	Nodes             []string `json:"nodes"`
	NodeType          string   `json:"node_type"`
}
type CheckAndType struct {
	CheckName string `json:"check_name"`
	CheckType string `json:"check_type"`
	CheckMsg  string `json:"check_msg"`
}

type ChecksResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}

func NewSuccessCheck(title string, successMsg string) *Checks {
	return &Checks{
		Title:         title,
		Passed:        true,
		SuccessMsg:    successMsg,
	}
}

func NewFailureCheck(title string, errorMsg string, resolutionMsg string) *Checks {
	return &Checks{
		Title:         title,
		Passed:        false,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}