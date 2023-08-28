package models

import (
	"errors"
	"log"
	"strconv"
	"strings"

	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/config"
	"github.com/gofiber/fiber/v2"
)

const (
	AWS_S3            = "s3"
	GCP_CLOUD_STORAGE = "gcs"
)

type BatchCheckRequest struct {
	Checks []string `json:"checks"`
	Config *Config  `json:"config"`
}

type MockServerFromBatchServiceResponse struct {
	Host       string `json:"host"`
	Protocol   string `json:"protocol"`
	Port       int    `json:"port"`
	Error      error  `json:"error"`
	StatusCode int    `json:"status_code"`
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

type ObjectStorage struct {
	Location                 string             `json:"location"`
	Endpoint                 string             `json:"endpoint"`
	BucketName               string             `json:"bucket_name"`
	BasePath                 string             `json:"base_path"`
	AccessKey                string             `json:"access_key"`
	SecretKey                string             `json:"secret_key"`
	AWSRegion                string             `json:"aws_region"`
	GoogleServiceAccountFile string             `json:"google_service_account_file"`
	GcpServiceAccount        *GcpServiceAccount `json:"gcp_service_account"`
}
type Backup struct {
	FileSystem    *FileSystem    `json:"file_system"`
	ObjectStorage *ObjectStorage `json:"object_storage"`
}

type Certificate struct {
	Fqdn         string      `json:"fqdn"`
	FqdnRootCert string      `json:"fqdn_root_ca"`
	NodeType     string      `json:"node_type"`
	Nodes        []*NodeCert `json:"nodes"`
}

type ExternalOS struct {
	OSDomainName                  string `json:"opensearch_domain_name"`
	OSDomainURL                   string `json:"opensearch_domain_url"`
	OSUsername                    string `json:"opensearch_username"`
	OSUserPassword                string `json:"opensearch_user_password"`
	OSCert                        string `json:"opensearch_cert"`
	OSRoleArn                     string `json:"opensearch_role_arn"`
	OsSnapshotUserAccessKeySecret string `json:"os_snapshot_user_access_key_secret"`
	OsSnapshotUserAccessKeyId     string `json:"os_snapshot_user_access_key_id"`
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
	SSHUser         *SSHUser       `json:"ssh_user"`
	Arch            string         `json:"arch"`
	Profile         string         `json:"profile"`
	Backup          *Backup        `json:"backup"`
	Hardware        *Hardware      `json:"hardware"`
	ExternalDbType  string         `json:"external_db_type"`
	Certificate     []*Certificate `json:"certificate"`
	ExternalOS      *ExternalOS    `json:"external_opensearch"`
	ExternalPG      *ExternalPG    `json:"external_postgresql"`
	DeploymentState string         `json:"deployment_state"`
	APIToken        string         `json:"api_token"`
}

func NewConfig() *Config {
	return &Config{
		Hardware:    &Hardware{},
		Certificate: []*Certificate{},
		SSHUser:     &SSHUser{},
	}
}

func appendCertsByIpToNodeCerts(certsByIP *[]config.CertByIP, ipList []string, privateKey, publicKey, adminKey, adminCert, nodeRootCa string) []*NodeCert {
	nodeCertsList := make([]*NodeCert, 0)
	certByIpMap := createMapforCertByIp(certsByIP)

	for _, ip := range ipList {
		certByIP, ok := certByIpMap[ip]
		var nodeCert *NodeCert
		if ok {
			nodeCert = &NodeCert{
				IP:        certByIP.IP,
				Key:       certByIP.PrivateKey,
				Cert:      certByIP.PublicKey,
				AdminKey:  adminKey,
				AdminCert: adminCert,
				RootCert:  nodeRootCa,
			}
		} else {
			nodeCert = &NodeCert{
				IP:        ip,
				Key:       privateKey,
				Cert:      publicKey,
				AdminKey:  adminKey,
				AdminCert: adminCert,
				RootCert:  nodeRootCa,
			}
		}
		nodeCertsList = append(nodeCertsList, nodeCert)
	}

	return nodeCertsList

}

func createMapforCertByIp(certsByIP *[]config.CertByIP) map[string]*config.CertByIP {
	certByIPMap := make(map[string]*config.CertByIP)
	if certsByIP != nil {
		for _, cert := range *certsByIP {
			certificateIp := cert
			certByIPMap[cert.IP] = &certificateIp
		}

	}
	return certByIPMap
}

func (c *Config) PopulateWith(haConfig *config.HaDeployConfig) error {
	if haConfig == nil {
		return nil
	}
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

	if (haConfig.GetConfigInitials().BackupConfig == "file_system" && c.Backup == nil) || (haConfig.GetConfigInitials().BackupConfig == "efs" && c.Backup == nil) {
		c.Backup = &Backup{
			FileSystem: &FileSystem{
				MountLocation: haConfig.GetConfigInitials().BackupMount,
			},
		}
	}

	if haConfig.GetObjectStorageConfig() != nil && haConfig.GetConfigInitials().BackupConfig == "object_storage" && c.Backup == nil {
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

	if haConfig == nil {
		return errors.New("haConfig is nil")
	}
	c.Hardware.AutomateNodeCount, err = strconv.Atoi(haConfig.Automate.Config.InstanceCount)
	if err != nil {
		return err
	}

	c.Hardware.ChefInfraServerNodeCount, err = strconv.Atoi(haConfig.ChefServer.Config.InstanceCount)
	if err != nil {
		return err
	}

	if haConfig.Postgresql.Config.InstanceCount == "" && haConfig.Aws.Config.SetupManagedServices {
		c.Hardware.PostgresqlNodeCount = 0
	} else {
		c.Hardware.PostgresqlNodeCount, err = strconv.Atoi(haConfig.Postgresql.Config.InstanceCount)
		if err != nil {
			return err
		}
	}

	if haConfig.Opensearch.Config.InstanceCount == "" && haConfig.Aws.Config.SetupManagedServices {
		c.Hardware.OpenSearchNodeCount = 0
	} else {
		c.Hardware.OpenSearchNodeCount, err = strconv.Atoi(haConfig.Opensearch.Config.InstanceCount)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Config) populateObjectStorageConfig(haConfig *config.HaDeployConfig) {
	if haConfig == nil {
		logrus.Errorln("haConfig cannot be nil")
		return
	}

	objectStorageConfig := haConfig.GetObjectStorageConfig()
	if objectStorageConfig.Location == "" {
		logrus.Errorln("object storage location cannot be empty")
		return
	}

	if objectStorageConfig.Location == AWS_S3 {
		c.Backup = &Backup{
			ObjectStorage: &ObjectStorage{
				BucketName: objectStorageConfig.BucketName,
				AWSRegion:  objectStorageConfig.Region,
				AccessKey:  objectStorageConfig.AccessKey,
				SecretKey:  objectStorageConfig.SecretKey,
				Endpoint:   objectStorageConfig.Endpoint,
			},
		}
	} else if objectStorageConfig.Location == GCP_CLOUD_STORAGE {
		c.Backup = &Backup{
			ObjectStorage: &ObjectStorage{
				BucketName:               objectStorageConfig.BucketName,
				GoogleServiceAccountFile: objectStorageConfig.GoogleServiceAccountFile,
				GcpServiceAccount: &GcpServiceAccount{
					Type:                    objectStorageConfig.GcpServiceAccount.Type,
					ProjectID:               objectStorageConfig.GcpServiceAccount.ProjectID,
					PrivateKeyID:            objectStorageConfig.GcpServiceAccount.PrivateKeyID,
					PrivateKey:              objectStorageConfig.GcpServiceAccount.PrivateKey,
					ClientEmail:             objectStorageConfig.GcpServiceAccount.ClientEmail,
					ClientID:                objectStorageConfig.GcpServiceAccount.ClientID,
					AuthURI:                 objectStorageConfig.GcpServiceAccount.AuthURI,
					TokenURI:                objectStorageConfig.GcpServiceAccount.TokenURI,
					AuthProviderX509CertURL: objectStorageConfig.GcpServiceAccount.AuthProviderX509CertURL,
					ClientX509CertURL:       objectStorageConfig.GcpServiceAccount.ClientX509CertURL,
					UniverseDomain:          objectStorageConfig.GcpServiceAccount.UniverseDomain,
				},
			},
		}
	} else {
		logrus.Errorf("invalid location: %s", objectStorageConfig.Location)
		return
	}
}

func (c *Config) populateConfigInitials(haConfig *config.HaDeployConfig) {
	configInitials := haConfig.GetConfigInitials()
	c.SSHUser.Username = configInitials.SSHUser
	c.SSHUser.Port = configInitials.SSHPort
	c.SSHUser.PrivateKey = configInitials.SSHKeyFile
	c.SSHUser.SudoPassword = configInitials.SudoPassword
	c.Arch = configInitials.Architecture
}

func (c *Config) populateAwsS3BucketName(haConfig *config.HaDeployConfig) {
	if haConfig.Architecture.Aws.BackupConfig == "s3" {
		c.Backup = &Backup{
			ObjectStorage: &ObjectStorage{
				BucketName: haConfig.Architecture.Aws.S3BucketName,
			},
		}
		c.Backup.ObjectStorage.AWSRegion = haConfig.Aws.Config.Region

		if haConfig.Aws.Config.Profile != "" {
			cred := credentials.NewSharedCredentials("", "")
			creds, err := cred.Get()
			if err != nil {
				log.Println("populateAwsS3BucketName:", err)
			}

			c.Backup.ObjectStorage.AccessKey = creds.AccessKeyID
			c.Backup.ObjectStorage.SecretKey = creds.SecretAccessKey
		}

	}
}

func (c *Config) populateAwsCerts(haConfig *config.HaDeployConfig) {
	automateConfig := haConfig.Automate.Config
	cert := addCertificatesInConfig(automateConfig.Fqdn, automateConfig.FqdnRootCA, config.AUTOMATE)
	if automateConfig.EnableCustomCerts {
		cert.Nodes = appendCertsByIpToNodeCerts(automateConfig.CertsByIP, c.Hardware.AutomateNodeIps, automateConfig.PrivateKey, automateConfig.PublicKey, "", "", "")
	}
	c.Certificate = append(c.Certificate, cert)
	chefserverConfig := haConfig.ChefServer.Config
	//We can populate this later as well in config
	cert = addCertificatesInConfig(chefserverConfig.ChefServerFqdn, chefserverConfig.FqdnRootCA, config.CHEFSERVER)
	if chefserverConfig.EnableCustomCerts {
		cert.Nodes = appendCertsByIpToNodeCerts(chefserverConfig.CertsByIP, c.Hardware.ChefInfraServerNodeIps, chefserverConfig.PrivateKey, chefserverConfig.PublicKey, "", "", "")
	}

	c.Certificate = append(c.Certificate, cert)

	postgresqlConfig := haConfig.Postgresql.Config
	if postgresqlConfig.EnableCustomCerts {
		cert = addCertificatesInConfig("", "", config.POSTGRESQL)
		cert.Nodes = appendCertsByIpToNodeCerts(postgresqlConfig.CertsByIP, c.Hardware.PostgresqlNodeIps, postgresqlConfig.PrivateKey, postgresqlConfig.PublicKey, "", "", postgresqlConfig.RootCA)
		c.Certificate = append(c.Certificate, cert)
	}

	openSearchConfig := haConfig.Opensearch.Config
	if openSearchConfig.EnableCustomCerts {
		cert = addCertificatesInConfig("", "", config.OPENSEARCH)
		cert.Nodes = appendCertsByIpToNodeCerts(openSearchConfig.CertsByIP, c.Hardware.OpenSearchNodeIps, openSearchConfig.PrivateKey, openSearchConfig.PublicKey, openSearchConfig.AdminKey, openSearchConfig.AdminCert, openSearchConfig.RootCA)
		c.Certificate = append(c.Certificate, cert)
	}
}

func (c *Config) populateAwsManagedServicesConfig(haConfig *config.HaDeployConfig) {
	awsManagedServicesConfig := haConfig.Aws.Config
	if c.ExternalPG == nil {
		c.ExternalPG = &ExternalPG{}
	}

	c.ExternalPG.PGDbUserName = awsManagedServicesConfig.ManagedRdsDbuserUsername
	c.ExternalPG.PGDbUserPassword = awsManagedServicesConfig.ManagedRdsDbuserPassword
	c.ExternalPG.PGInstanceURL = awsManagedServicesConfig.ManagedRdsInstanceURL
	c.ExternalPG.PGRootCert = awsManagedServicesConfig.ManagedRdsCertificate
	if len(strings.TrimSpace(awsManagedServicesConfig.ManagedRdsCertificate)) < 1 {
		c.ExternalPG.PGRootCert = EXTERNAL_PG_ROOT_CERT
	}
	c.ExternalPG.PGSuperuserName = awsManagedServicesConfig.ManagedRdsSuperuserUsername
	c.ExternalPG.PGSuperuserPassword = awsManagedServicesConfig.ManagedRdsSuperuserPassword

	if c.ExternalOS == nil {
		c.ExternalOS = &ExternalOS{}
	}

	c.ExternalOS.OSDomainName = awsManagedServicesConfig.ManagedOpensearchDomainName
	c.ExternalOS.OSDomainURL = awsManagedServicesConfig.ManagedOpensearchDomainURL
	c.ExternalOS.OSCert = awsManagedServicesConfig.ManagedOpensearchCertificate
	if len(strings.TrimSpace(awsManagedServicesConfig.ManagedOpensearchCertificate)) < 1 {
		c.ExternalOS.OSCert = EXTERNAL_OPENSEARCH_ROOT_CERT
	}
	c.ExternalOS.OSUserPassword = awsManagedServicesConfig.ManagedOpensearchUserPassword
	c.ExternalOS.OSUsername = awsManagedServicesConfig.ManagedOpensearchUsername
}

func (c *Config) populateExistingInfraCommonConfig(haConfig *config.HaDeployConfig) {
	existingInfraConfig := haConfig.ExistingInfra.Config
	c.Hardware.AutomateNodeIps = existingInfraConfig.AutomatePrivateIps
	c.Hardware.ChefInfraServerNodeIps = existingInfraConfig.ChefServerPrivateIps

	//Adding Certificate for automate FQDN and nodes certificates
	automateConfig := haConfig.Automate.Config
	cert := addCertificatesInConfig(automateConfig.Fqdn, automateConfig.FqdnRootCA, config.AUTOMATE)
	if automateConfig.EnableCustomCerts {
		//Adding ip node certs
		cert.Nodes = appendCertsByIpToNodeCerts(automateConfig.CertsByIP, haConfig.ExistingInfra.Config.AutomatePrivateIps, automateConfig.PrivateKey, automateConfig.PublicKey, "", "", "")
	}
	c.Certificate = append(c.Certificate, cert)

	//Adding Certificates for chef server FQDN and nodes certificates
	chefserverConfig := haConfig.ChefServer.Config
	cert = addCertificatesInConfig(chefserverConfig.ChefServerFqdn, chefserverConfig.FqdnRootCA, config.CHEFSERVER)
	if chefserverConfig.EnableCustomCerts {
		cert.Nodes = appendCertsByIpToNodeCerts(chefserverConfig.CertsByIP, haConfig.ExistingInfra.Config.ChefServerPrivateIps, chefserverConfig.PrivateKey, chefserverConfig.PublicKey, "", "", "")
	}
	c.Certificate = append(c.Certificate, cert)
}

func addCertificatesInConfig(fqdn, rootCA, nodeType string) *Certificate {
	cert := &Certificate{
		Fqdn:         fqdn,
		FqdnRootCert: rootCA,
		NodeType:     nodeType,
	}

	return cert
}

func (c *Config) populateExternalDbConfig(haConfig *config.HaDeployConfig) {
	c.ExternalDbType = haConfig.External.Database.Type
	externalPgConfig := haConfig.External.Database.PostgreSQL
	if c.ExternalPG == nil {
		c.ExternalPG = &ExternalPG{}
	}
	c.ExternalPG.PGDbUserName = externalPgConfig.DbuserUsername
	c.ExternalPG.PGDbUserPassword = externalPgConfig.DbuserPassword
	c.ExternalPG.PGInstanceURL = externalPgConfig.InstanceURL
	c.ExternalPG.PGRootCert = externalPgConfig.PostgresqlRootCert

	if len(externalPgConfig.PostgresqlRootCert) < 1 && haConfig.External.Database.Type == "aws" {
		c.ExternalPG.PGRootCert = EXTERNAL_PG_ROOT_CERT
	}

	c.ExternalPG.PGSuperuserName = externalPgConfig.SuperuserUsername
	c.ExternalPG.PGSuperuserPassword = externalPgConfig.SuperuserPassword

	externalOsConfig := haConfig.External.Database.OpenSearch

	if c.ExternalOS == nil {
		c.ExternalOS = &ExternalOS{}
	}

	c.ExternalOS.OSDomainName = externalOsConfig.OpensearchDomainName
	c.ExternalOS.OSDomainURL = externalOsConfig.OpensearchDomainURL
	c.ExternalOS.OSCert = externalOsConfig.OpensearchRootCert

	if len(externalOsConfig.OpensearchRootCert) < 1 && haConfig.External.Database.Type == "aws" {
		c.ExternalOS.OSCert = EXTERNAL_OPENSEARCH_ROOT_CERT
	}
	c.ExternalOS.OSUserPassword = externalOsConfig.OpensearchUserPassword
	c.ExternalOS.OSUsername = externalOsConfig.OpensearchUsername

	if haConfig.IsAwsExternalOsConfigured() {
		c.ExternalOS.OSRoleArn = externalOsConfig.Aws.AwsOsSnapshotRoleArn
		c.ExternalOS.OsSnapshotUserAccessKeyId = externalOsConfig.Aws.OsSnapshotUserAccessKeyID
		c.ExternalOS.OsSnapshotUserAccessKeySecret = externalOsConfig.Aws.OsSnapshotUserAccessKeySecret
	}
}

func (c *Config) populateNonExternalDbConfig(haConfig *config.HaDeployConfig) {
	existingInfraConfig := haConfig.ExistingInfra.Config
	c.Hardware.PostgresqlNodeIps = existingInfraConfig.PostgresqlPrivateIps
	c.Hardware.OpenSearchNodeIps = existingInfraConfig.OpensearchPrivateIps
	postgresqlConfig := haConfig.Postgresql.Config
	if postgresqlConfig.EnableCustomCerts {
		postgresqlCert := addCertificatesInConfig("", "", config.POSTGRESQL)
		postgresqlCert.Nodes = appendCertsByIpToNodeCerts(postgresqlConfig.CertsByIP, haConfig.ExistingInfra.Config.PostgresqlPrivateIps, postgresqlConfig.PrivateKey, postgresqlConfig.PublicKey, "", "", postgresqlConfig.RootCA)
		c.Certificate = append(c.Certificate, postgresqlCert)

	}

	openSearchConfig := haConfig.Opensearch.Config
	//As their is no FQDN for opensearch but we have fqdn root ca
	if openSearchConfig.EnableCustomCerts {
		openSearchCertConfig := addCertificatesInConfig("", "", config.OPENSEARCH)
		openSearchCertConfig.Nodes = appendCertsByIpToNodeCerts(openSearchConfig.CertsByIP, haConfig.ExistingInfra.Config.OpensearchPrivateIps, openSearchConfig.PrivateKey, openSearchConfig.PublicKey, openSearchConfig.AdminKey, openSearchConfig.AdminCert, openSearchConfig.RootCA)
		c.Certificate = append(c.Certificate, openSearchCertConfig)
	}
}

type BatchCheckResponse struct {
	Passed     bool               `json:"passed"`
	NodeResult []BatchCheckResult `json:"node_result"`
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
	Passed      bool         `json:"passed"`
	Message     string       `json:"msg"`
	Check       string       `json:"check"`
	Checks      []Checks     `json:"checks"`
	Error       *fiber.Error `json:"error,omitempty"`
	Skipped     bool         `json:"skipped"`
	SkipMessage string       `json:"skip_message,omitempty"`
}

type Checks struct {
	Title         string `json:"title"`
	Passed        bool   `json:"passed"`
	SuccessMsg    string `json:"success_msg"`
	ErrorMsg      string `json:"error_msg"`
	ResolutionMsg string `json:"resolution_msg"`
	Skipped       bool   `json:"skipped"`
}

// is this supposed to be cert_by_ip? this struct needs modifiation
type NodeCert struct {
	IP        string `json:"ip"`
	RootCert  string `json:"root_cert"`
	Cert      string `json:"cert"`
	Key       string `json:"key"`
	AdminKey  string `json:"admin_key"`
	AdminCert string `json:"admin_cert"`
}

type HardwareResourceCheckResponse struct {
	Status string                           `json:"status"`
	Result []HardwareResourceCountApiResult `json:"result"`
	Error  fiber.Error                      `json:"error"`
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
	Error  fiber.Error        `json:"error"`
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
