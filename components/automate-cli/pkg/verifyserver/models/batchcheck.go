package models

import (
	"strconv"

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

func (c *Config) appendCertsByIpToNodeCerts(certsByIP *[]config.CertByIP, optionalArgs ...string) {
	var rootCA, adminKey, adminCert string

	if len(optionalArgs) > 0 {
		rootCA = optionalArgs[0]
	}
	if len(optionalArgs) > 1 {
		adminKey = optionalArgs[1]
	}
	if len(optionalArgs) > 2 {
		adminCert = optionalArgs[2]
	}

	if certsByIP != nil {
		for _, certByIP := range *certsByIP {
			nodeCert := NodeCert{
				IP:   certByIP.IP,
				Key:  certByIP.PublicKey,
				Cert: rootCA,
			}
			if adminKey != "" {
				nodeCert.AdminKey = adminKey
			}
			if adminCert != "" {
				nodeCert.AdminCert = adminCert
			}
			c.Certificate.Nodes = append(c.Certificate.Nodes, nodeCert)
		}
	}
}

func (c *Config) PopulateWith(haConfig *config.HaDeployConfig) error {
	err := haConfig.Verify()
	if err != nil {
		return err
	}
	if haConfig.GetConfigInitials() != nil {
		configInitials := haConfig.GetConfigInitials()
		c.SSHUser.Username = configInitials.SSHUser
		c.SSHUser.PrivateKey = configInitials.SSHKeyFile
		c.SSHUser.SudoPassword = configInitials.SudoPassword
		c.Arch = configInitials.Architecture
		c.Backup.FileSystem.MountLocation = configInitials.BackupMount
	}

	if haConfig.GetObjectStorageConfig() != nil {
		objectStorageConfig := haConfig.GetObjectStorageConfig()
		c.Backup.ObjectStorage.BucketName = objectStorageConfig.BucketName
		c.Backup.ObjectStorage.AWSRegion = objectStorageConfig.Region
		c.Backup.ObjectStorage.AccessKey = objectStorageConfig.AccessKey
		c.Backup.ObjectStorage.SecretKey = objectStorageConfig.SecretKey
		c.Backup.ObjectStorage.Endpoint = objectStorageConfig.Endpoint
	}

	c.Hardware.AutomateNodeCount, _ = strconv.Atoi(haConfig.Automate.Config.InstanceCount)
	c.Hardware.ChefInfraServerNodeCount, _ = strconv.Atoi(haConfig.ChefServer.Config.InstanceCount)
	c.Hardware.PostgresqlNodeCount, _ = strconv.Atoi(haConfig.Postgresql.Config.InstanceCount)
	c.Hardware.OpenSearchNodeCount, _ = strconv.Atoi(haConfig.Opensearch.Config.InstanceCount)
	c.Certificate.RootCert = haConfig.Automate.Config.RootCA
	c.Certificate.AutomateFqdn = haConfig.Automate.Config.Fqdn
	// pre deploy state cs fqdn is same as automate fqdn
	c.Certificate.ChefServerFqdn = haConfig.Automate.Config.Fqdn

	if haConfig.IsExistingInfra() {
		existingInfraConfig := haConfig.ExistingInfra.Config
		c.Hardware.AutomateNodeIps = existingInfraConfig.AutomatePrivateIps
		c.Hardware.ChefInfraServerNodeIps = existingInfraConfig.ChefServerPrivateIps
		c.appendCertsByIpToNodeCerts(haConfig.Automate.Config.CertsByIP, haConfig.Automate.Config.RootCA)
		c.appendCertsByIpToNodeCerts(haConfig.ChefServer.Config.CertsByIP)

		if !haConfig.IsExternalDb() {
			c.Hardware.PostgresqlNodeIps = existingInfraConfig.PostgresqlPrivateIps
			c.Hardware.OpenSearchNodeIps = existingInfraConfig.OpensearchPrivateIps
			postgresqlConfig := haConfig.Postgresql.Config
			c.appendCertsByIpToNodeCerts(postgresqlConfig.CertsByIP, postgresqlConfig.RootCA)
			openSearchConfig := haConfig.Opensearch.Config
			c.appendCertsByIpToNodeCerts(openSearchConfig.CertsByIP, openSearchConfig.RootCA, openSearchConfig.AdminKey, openSearchConfig.AdminCert)
		}

		if haConfig.IsExternalDb() {

			externalPgConfig := haConfig.External.Database.PostgreSQL
			c.ExternalPG.PGDbUserName = externalPgConfig.DbuserUsername
			c.ExternalPG.PGDbUserPassword = externalPgConfig.DbuserPassword
			c.ExternalPG.PGInstanceURL = externalPgConfig.InstanceURL

			// pg root-ca might be nil in pre deploy state
			c.ExternalPG.PGRootCert = externalPgConfig.PostgresqlRootCert
			c.ExternalPG.PGSuperuserName = externalPgConfig.SuperuserUsername
			c.ExternalPG.PGSuperuserPassword = externalPgConfig.SuperuserPassword

			externalOsConfig := haConfig.External.Database.OpenSearch
			c.ExternalOS.OSDomainName = externalOsConfig.OpensearchDomainName
			c.ExternalOS.OSDomainURL = externalOsConfig.OpensearchDomainURL

			// os root-ca might be nil in pre deploy state
			c.ExternalOS.OSCert = externalOsConfig.OpensearchRootCert
			c.ExternalOS.OSUserPassword = externalOsConfig.OpensearchUserPassword
			c.ExternalOS.OSUsername = externalOsConfig.OpensearchUsername

			if haConfig.IsAwsExternalOsConfigured() {
				c.ExternalOS.OSRoleArn = externalOsConfig.Aws.AwsOsSnapshotRoleArn
			}
		}
	}

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