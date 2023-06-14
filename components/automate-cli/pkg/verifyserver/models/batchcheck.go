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
	Port         string `json:"ssh_port"`
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

func (c *Config) appendCertsByIpToNodeCerts(certsByIP *[]config.CertByIP, ipList []string, privateKey, publicKey, adminKey, adminCert string) {

	for _, ip := range ipList {
		nodeCert := NodeCert{
			IP:        ip,
			Key:       privateKey,
			Cert:      publicKey,
			AdminKey:  adminKey,
			AdminCert: adminCert,
		}
		c.Certificate.Nodes = append(c.Certificate.Nodes, nodeCert)
	}

	if certsByIP != nil {
		for _, certByIP := range *certsByIP {
			nodeCert := NodeCert{
				IP:        certByIP.IP,
				Key:       certByIP.PrivateKey,
				Cert:      certByIP.PublicKey,
				AdminKey:  adminKey,
				AdminCert: adminCert,
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

	if err = c.populateCommonConfig(haConfig); err != nil {
		return err
	}

	if haConfig.GetConfigInitials() != nil {
		c.populateConfigInitials(haConfig)
	}

	if haConfig.GetObjectStorageConfig() != nil {
		c.populateObjectStorageConfig(haConfig)
	}

	if haConfig.IsAws() {
		c.populateAwsConfig(haConfig)
	}

	if haConfig.IsExistingInfra() {
		c.populateExistingInfraConfig(haConfig)
	}

	// not available in config
	c.DeploymentState = ""

	return nil
}

func (c *Config) populateAwsConfig(haConfig *config.HaDeployConfig) {
	c.populateAwsS3BucketName(haConfig)
	c.populateAwsCerts(haConfig)

	if haConfig.Aws.Config.SetupManagedServices {
		c.populateAwsManagedServicesConfig(haConfig)
	}
}

func (c *Config) populateExistingInfraConfig(haConfig *config.HaDeployConfig) {
	c.populateExistingInfraCommonConfig(haConfig)

	if !haConfig.IsExternalDb() {
		c.populateNonExternalDbConfig(haConfig)
	}

	if haConfig.IsExternalDb() {
		c.populateExternalDbConfig(haConfig)
	}
}

func (c *Config) populateCommonConfig(haConfig *config.HaDeployConfig) error {
	var err error
	c.Hardware.AutomateNodeCount, err = strconv.Atoi(haConfig.Automate.Config.InstanceCount)
	if err != nil {
		return err
	}

	c.Hardware.ChefInfraServerNodeCount, err = strconv.Atoi(haConfig.ChefServer.Config.InstanceCount)
	if err != nil {
		return err
	}

	c.Hardware.PostgresqlNodeCount, err = strconv.Atoi(haConfig.Postgresql.Config.InstanceCount)
	if err != nil {
		return err
	}

	c.Hardware.OpenSearchNodeCount, err = strconv.Atoi(haConfig.Opensearch.Config.InstanceCount)
	if err != nil {
		return err
	}

	c.Certificate.RootCert = haConfig.Automate.Config.RootCA
	c.Certificate.AutomateFqdn = haConfig.Automate.Config.Fqdn
	// pre deploy state cs fqdn is same as automate fqdn
	c.Certificate.ChefServerFqdn = haConfig.Automate.Config.Fqdn

	return nil
}

func (c *Config) populateObjectStorageConfig(haConfig *config.HaDeployConfig) {
	objectStorageConfig := haConfig.GetObjectStorageConfig()
	c.Backup.ObjectStorage.BucketName = objectStorageConfig.BucketName
	c.Backup.ObjectStorage.AWSRegion = objectStorageConfig.Region
	c.Backup.ObjectStorage.AccessKey = objectStorageConfig.AccessKey
	c.Backup.ObjectStorage.SecretKey = objectStorageConfig.SecretKey
	c.Backup.ObjectStorage.Endpoint = objectStorageConfig.Endpoint
}

func (c *Config) populateConfigInitials(haConfig *config.HaDeployConfig) {
	configInitials := haConfig.GetConfigInitials()
	c.SSHUser.Username = configInitials.SSHUser
	c.SSHUser.Port = configInitials.SSHPort
	c.SSHUser.PrivateKey = configInitials.SSHKeyFile
	c.SSHUser.SudoPassword = configInitials.SudoPassword
	c.Arch = configInitials.Architecture
	c.Backup.FileSystem.MountLocation = configInitials.BackupMount
}

func (c *Config) populateAwsS3BucketName(haConfig *config.HaDeployConfig) {
	if haConfig.Architecture.Aws.BackupConfig == "s3" {
		c.Backup.ObjectStorage.BucketName = haConfig.Architecture.Aws.S3BucketName
	}
}

func (c *Config) populateAwsCerts(haConfig *config.HaDeployConfig) {
	automateConfig := haConfig.Automate.Config
	if automateConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(nil, []string{""}, automateConfig.PrivateKey, automateConfig.PublicKey, "", "")
	}

	chefserverConfig := haConfig.ChefServer.Config
	if chefserverConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(nil, []string{""}, chefserverConfig.PrivateKey, chefserverConfig.PublicKey, "", "")
	}

	postgresqlConfig := haConfig.Postgresql.Config
	if postgresqlConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(nil, []string{""}, postgresqlConfig.PrivateKey, postgresqlConfig.PublicKey, "", "")
	}

	openSearchConfig := haConfig.Opensearch.Config
	if openSearchConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(nil, []string{""}, openSearchConfig.PrivateKey, openSearchConfig.PublicKey, openSearchConfig.AdminKey, openSearchConfig.AdminCert)
	}
}

func (c *Config) populateAwsManagedServicesConfig(haConfig *config.HaDeployConfig) {
	awsManagedServicesConfig := haConfig.Aws.Config
	c.ExternalPG.PGDbUserName = awsManagedServicesConfig.ManagedRdsDbuserUsername
	c.ExternalPG.PGDbUserPassword = awsManagedServicesConfig.ManagedRdsDbuserPassword
	c.ExternalPG.PGInstanceURL = awsManagedServicesConfig.ManagedRdsInstanceURL

	// pg root-ca might be nil in pre deploy state
	c.ExternalPG.PGRootCert = awsManagedServicesConfig.ManagedRdsCertificate
	c.ExternalPG.PGSuperuserName = awsManagedServicesConfig.ManagedRdsSuperuserUsername
	c.ExternalPG.PGSuperuserPassword = awsManagedServicesConfig.ManagedRdsSuperuserPassword

	c.ExternalOS.OSDomainName = awsManagedServicesConfig.ManagedOpensearchDomainName
	c.ExternalOS.OSDomainURL = awsManagedServicesConfig.ManagedOpensearchDomainURL

	// os root-ca might be nil in pre deploy state
	c.ExternalOS.OSCert = awsManagedServicesConfig.ManagedOpensearchCertificate
	c.ExternalOS.OSUserPassword = awsManagedServicesConfig.ManagedOpensearchUserPassword
	c.ExternalOS.OSUsername = awsManagedServicesConfig.ManagedOpensearchUsername
	c.ExternalOS.OSRoleArn = awsManagedServicesConfig.AwsOsSnapshotRoleArn
}

func (c *Config) populateExistingInfraCommonConfig(haConfig *config.HaDeployConfig) {
	existingInfraConfig := haConfig.ExistingInfra.Config
	c.Hardware.AutomateNodeIps = existingInfraConfig.AutomatePrivateIps
	c.Hardware.ChefInfraServerNodeIps = existingInfraConfig.ChefServerPrivateIps
	automateConfig := haConfig.Automate.Config
	if automateConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(automateConfig.CertsByIP, haConfig.ExistingInfra.Config.AutomatePrivateIps, automateConfig.PrivateKey, automateConfig.PublicKey, "", "")
	}

	chefserverConfig := haConfig.ChefServer.Config
	if chefserverConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(chefserverConfig.CertsByIP, haConfig.ExistingInfra.Config.ChefServerPrivateIps, chefserverConfig.PrivateKey, chefserverConfig.PublicKey, "", "")
	}
}

func (c *Config) populateExternalDbConfig(haConfig *config.HaDeployConfig) {
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

func (c *Config) populateNonExternalDbConfig(haConfig *config.HaDeployConfig) {
	existingInfraConfig := haConfig.ExistingInfra.Config
	c.Hardware.PostgresqlNodeIps = existingInfraConfig.PostgresqlPrivateIps
	c.Hardware.OpenSearchNodeIps = existingInfraConfig.OpensearchPrivateIps
	postgresqlConfig := haConfig.Postgresql.Config
	if postgresqlConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(postgresqlConfig.CertsByIP, haConfig.ExistingInfra.Config.PostgresqlPrivateIps, postgresqlConfig.PrivateKey, postgresqlConfig.PublicKey, "", "")
	}

	openSearchConfig := haConfig.Opensearch.Config
	if openSearchConfig.EnableCustomCerts {
		c.appendCertsByIpToNodeCerts(openSearchConfig.CertsByIP, haConfig.ExistingInfra.Config.OpensearchPrivateIps, openSearchConfig.PrivateKey, openSearchConfig.PublicKey, openSearchConfig.AdminKey, openSearchConfig.AdminCert)
	}
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
	Port         string `json:"ssh_port"`
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
		Title:      title,
		Passed:     true,
		SuccessMsg: successMsg,
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
