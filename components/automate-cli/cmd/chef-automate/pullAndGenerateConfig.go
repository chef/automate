package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	mtoml "github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/toml"
	"github.com/sirupsen/logrus"
)

type ConfigKeys struct {
	rootCA     string
	privateKey string
	publicKey  string
	adminCert  string
	adminKey   string
}

type ObjectStorageConfig struct {
	accessKey  string
	secrectKey string
	endpoint   string
	bucketName string
	RoleArn    string
}

type HAAwsAutoTfvars struct {
	AwsProfile                         string      `json:"aws_profile"`
	AwsRegion                          string      `json:"aws_region"`
	Endpoint                           string      `json:"endpoint"`
	SecretKey                          string      `json:"secret_key"`
	AccessKey                          string      `json:"access_key"`
	BucketName                         string      `json:"bucket_name"`
	Architecture                       string      `json:"architecture"`
	SshKeyFileName                     string      `json:"aws_ssh_key_pair_name"`
	AwsSshKeyFile                      string      `json:"ssh_key_file"`
	SshPort                            string      `json:"ssh_port"`
	SshUser                            string      `json:"ssh_user"`
	HabitatUidGid                      string      `json:"habitat_uid_gid"`
	PostgresqlArchiveDiskFsPath        string      `json:"postgresql_archive_disk_fs_path"`
	PostgresqlInstanceCount            int         `json:"postgresql_instance_count"`
	NfsMountPath                       string      `json:"nfs_mount_path"`
	OpensearchCertsByIp                interface{} `json:"opensearch_certs_by_ip"`
	PostgresqlCertsByIp                interface{} `json:"postgresql_certs_by_ip"`
	ChefServerCertsByIp                interface{} `json:"chef_server_certs_by_ip"`
	AutomateCertsByIp                  interface{} `json:"automate_certs_by_ip"`
	OpensearchNodesDn                  string      `json:"opensearch_nodes_dn"`
	OpensearchAdminDn                  string      `json:"opensearch_admin_dn"`
	OpensearchCustomCertsEnabled       bool        `json:"opensearch_custom_certs_enabled"`
	PostgresqlCustomCertsEnabled       bool        `json:"postgresql_custom_certs_enabled"`
	ChefServerCustomCertsEnabled       bool        `json:"chef_server_custom_certs_enabled"`
	AutomateCustomCertsEnabled         bool        `json:"automate_custom_certs_enabled"`
	PostgresqlPublicKey                string      `json:"postgresql_public_key"`
	OpensearchAdminCert                string      `json:"opensearch_admin_cert"`
	OpensearchPublicKey                string      `json:"opensearch_public_key"`
	ChefServerPublicKey                string      `json:"chef_server_public_key"`
	AutomatePublicKey                  string      `json:"automate_public_key"`
	PostgresqlPrivateKey               string      `json:"postgresql_private_key"`
	OpensearchPrivateKey               string      `json:"opensearch_private_key"`
	OpensearchAdminKey                 string      `json:"opensearch_admin_key"`
	ChefServerPrivateKey               string      `json:"chef_server_private_key"`
	AutomatePrivateKey                 string      `json:"automate_private_key"`
	PostgresqlRootCa                   string      `json:"postgresql_root_ca"`
	OpensearchRootCa                   string      `json:"opensearch_root_ca"`
	AutomateRootCa                     string      `json:"automate_root_ca"`
	OpensearchInstanceCount            int         `json:"opensearch_instance_count"`
	ChefServerInstanceCount            int         `json:"chef_server_instance_count"`
	AutomateInstanceCount              int         `json:"automate_instance_count"`
	AutomateFqdn                       string      `json:"automate_fqdn"`
	AutomateConfigFile                 string      `json:"automate_config_file"`
	OpensearchRootCert                 string      `json:"opensearch_root_cert"`
	PostgresqlRootCert                 string      `json:"postgresql_root_cert"`
	AwsVpcId                           string      `json:"aws_vpc_id"`
	AmiID                              string      `json:"aws_ami_id"`
	AwsCidrBlockAddr                   string      `json:"aws_cidr_block_addr"`
	PrivateCustomSubnets               []string    `json:"private_custom_subnets"`
	PublicCustomSubnets                []string    `json:"public_custom_subnets"`
	AutomateLbCertificateArn           string      `json:"automate_lb_certificate_arn"`
	ChefServerLbCertificateArn         string      `json:"chef_server_lb_certificate_arn"`
	AwsManagedRdsPostgresqlCertificate string      `json:"managed_rds_certificate"`
	ManagedRdsDbuserPassword           string      `json:"managed_rds_dbuser_password"`
	ManagedRdsDbuserUsername           string      `json:"managed_rds_dbuser_username"`
	ManagedRdsSuperuserPassword        string      `json:"managed_rds_superuser_password"`
	ManagedRdsSuperuserUsername        string      `json:"managed_rds_superuser_username"`
	ManagedRdsInstanceUrl              string      `json:"managed_rds_instance_url"`
	OsSnapshotUserAccessKeySecret      string      `json:"os_snapshot_user_access_key_secret"`
	OsSnapshotUserAccessKeyId          string      `json:"os_snapshot_user_access_key_id"`
	AwsOsSnapshotRoleArn               string      `json:"aws_os_snapshot_role_arn"`
	ManagedOpensearchCertificate       string      `json:"managed_opensearch_certificate"`
	ManagedOpensearchUserPassword      string      `json:"managed_opensearch_user_password"`
	ManagedOpensearchUsername          string      `json:"managed_opensearch_username"`
	ManagedOpensearchDomainUrl         string      `json:"managed_opensearch_domain_url"`
	ManagedOpensearchDomainName        string      `json:"managed_opensearch_domain_name"`
	AwsSetupManagedServices            bool        `json:"setup_managed_services"`
	ExistingPostgresqlPrivateIps       []string    `json:"existing_postgresql_private_ips"`
	ExistingOpensearchPrivateIps       []string    `json:"existing_opensearch_private_ips"`
	ExistingChefServerPrivateIps       []string    `json:"existing_chef_server_private_ips"`
	ExistingAutomatePrivateIps         []string    `json:"existing_automate_private_ips"`
	BackupConfigS3                     string      `json:"backup_config_s3"`
	BackupConfigEFS                    string      `json:"backup_config_efs"`
	AutomateAdminPassword              string      `json:"automate_admin_password"`
	LBAccessLogs                       string      `json:"lb_access_logs"`
	DeleteOnTermination                string      `json:"delete_on_termination"`
	AutomateServerInstanceType         string      `json:"automate_server_instance_type"`
	AutomateEbsVolumeIops              string      `json:"automate_ebs_volume_iops"`
	AutomateEbsVolumeSize              string      `json:"automate_ebs_volume_size"`
	AutomateEbsVolumeType              string      `json:"automate_ebs_volume_type"`
	ChefServerInstanceType             string      `json:"chef_server_instance_type"`
	ChefEbsVolumeIops                  string      `json:"chef_ebs_volume_iops"`
	ChefEbsVolumeSize                  string      `json:"chef_ebs_volume_size"`
	ChefEbsVolumeType                  string      `json:"chef_ebs_volume_type"`
	OpensearchServerInstanceType       string      `json:"opensearch_server_instance_type"`
	OpensearchEbsVolumeIops            string      `json:"opensearch_ebs_volume_iops"`
	OpensearchEbsVolumeSize            string      `json:"opensearch_ebs_volume_size"`
	OpensearchEbsVolumeType            string      `json:"opensearch_ebs_volume_type"`
	PostgresqlServerInstanceType       string      `json:"postgresql_server_instance_type"`
	PostgresqlEbsVolumeIops            string      `json:"postgresql_ebs_volume_iops"`
	PostgresqlEbsVolumeSize            string      `json:"postgresql_ebs_volume_size"`
	PostgresqlEbsVolumeType            string      `json:"postgresql_ebs_volume_type"`
}

type HATfvars struct {
	Region                          string      `json:"region"`
	Endpoint                        string      `json:"endpoint"`
	SecretKey                       string      `json:"secret_key"`
	AccessKey                       string      `json:"access_key"`
	BucketName                      string      `json:"bucket_name"`
	SshKeyFile                      string      `json:"ssh_key_file"`
	SshPort                         string      `json:"ssh_port"`
	SshUser                         string      `json:"ssh_user"`
	HabitatUidGid                   string      `json:"habitat_uid_gid"`
	PostgresqlArchiveDiskFsPath     string      `json:"postgresql_archive_disk_fs_path"`
	PostgresqlInstanceCount         int         `json:"postgresql_instance_count"`
	NfsMountPath                    string      `json:"nfs_mount_path"`
	OpensearchCertsByIp             interface{} `json:"opensearch_certs_by_ip"`
	PostgresqlCertsByIp             interface{} `json:"postgresql_certs_by_ip"`
	ChefServerCertsByIp             interface{} `json:"chef_server_certs_by_ip"`
	AutomateCertsByIp               interface{} `json:"automate_certs_by_ip"`
	OpensearchNodesDn               string      `json:"opensearch_nodes_dn"`
	OpensearchAdminDn               string      `json:"opensearch_admin_dn"`
	OpensearchCustomCertsEnabled    bool        `json:"opensearch_custom_certs_enabled"`
	PostgresqlCustomCertsEnabled    bool        `json:"postgresql_custom_certs_enabled"`
	ChefServerCustomCertsEnabled    bool        `json:"chef_server_custom_certs_enabled"`
	AutomateCustomCertsEnabled      bool        `json:"automate_custom_certs_enabled"`
	PostgresqlPublicKey             string      `json:"postgresql_public_key"`
	OpensearchAdminCert             string      `json:"opensearch_admin_cert"`
	OpensearchPublicKey             string      `json:"opensearch_public_key"`
	ChefServerPublicKey             string      `json:"chef_server_public_key"`
	AutomatePublicKey               string      `json:"automate_public_key"`
	PostgresqlPrivateKey            string      `json:"postgresql_private_key"`
	OpensearchPrivateKey            string      `json:"opensearch_private_key"`
	OpensearchAdminKey              string      `json:"opensearch_admin_key"`
	ChefServerPrivateKey            string      `json:"chef_server_private_key"`
	AutomatePrivateKey              string      `json:"automate_private_key"`
	PostgresqlRootCa                string      `json:"postgresql_root_ca"`
	OpensearchRootCa                string      `json:"opensearch_root_ca"`
	AutomateRootCa                  string      `json:"automate_root_ca"`
	OpensearchInstanceCount         int         `json:"opensearch_instance_count"`
	ChefServerInstanceCount         int         `json:"chef_server_instance_count"`
	AutomateInstanceCount           int         `json:"automate_instance_count"`
	AutomateFqdn                    string      `json:"automate_fqdn"`
	AutomateConfigFile              string      `json:"automate_config_file"`
	OpensearchRootCert              string      `json:"opensearch_root_cert"`
	PostgresqlRootCert              string      `json:"postgresql_root_cert"`
	AwsVpcId                        string      `json:"aws_vpc_id"`
	AmiID                           string      `json:"ami_id"`
	AwsCidrBlockAddr                string      `json:"aws_cidr_block_addr"`
	PrivateCustomSubnets            []string    `json:"private_custom_subnets"`
	PublicCustomSubnets             []string    `json:"public_custom_subnets"`
	SSHKeyPairName                  string      `json:"ssh_key_pair_name"`
	ManagedRdsPostgresqlCertificate string      `json:"managed_rds_postgresql_certificate"`
	ManagedRdsDbuserPassword        string      `json:"managed_rds_dbuser_password"`
	ManagedRdsDbuserUsername        string      `json:"managed_rds_dbuser_username"`
	ManagedRdsSuperuserPassword     string      `json:"managed_rds_superuser_password"`
	ManagedRdsSuperuserUsername     string      `json:"managed_rds_superuser_username"`
	ManagedRdsInstanceUrl           string      `json:"managed_rds_instance_url"`
	OsSnapshotUserAccessKeySecret   string      `json:"os_snapshot_user_access_key_secret"`
	OsSnapshotUserAccessKeyId       string      `json:"os_snapshot_user_access_key_id"`
	AwsOsSnapshotRoleArn            string      `json:"aws_os_snapshot_role_arn"`
	ManagedOpensearchCertificate    string      `json:"managed_opensearch_certificate"`
	ManagedOpensearchUserPassword   string      `json:"managed_opensearch_user_password"`
	ManagedOpensearchUsername       string      `json:"managed_opensearch_username"`
	ManagedOpensearchDomainUrl      string      `json:"managed_opensearch_domain_url"`
	ManagedOpensearchDomainName     string      `json:"managed_opensearch_domain_name"`
	SetupSelfManagedServices        string      `json:"setup_self_managed_services"`
	SetupManagedServices            string      `json:"setup_managed_services"`
	ExistingPostgresqlPrivateIps    []string    `json:"existing_postgresql_private_ips"`
	ExistingOpensearchPrivateIps    []string    `json:"existing_opensearch_private_ips"`
	ExistingChefServerPrivateIps    []string    `json:"existing_chef_server_private_ips"`
	ExistingAutomatePrivateIps      []string    `json:"existing_automate_private_ips"`
	BackupConfigS3                  string      `json:"backup_config_s3"`
	BackupConfigEFS                 string      `json:"backup_config_efs"`
	AutomateAdminPassword           string      `json:"automate_admin_password"`
	TeamsPort                       int         `json:"teams_port"`
	SecretsKeyFile                  string      `json:"secrets_key_file"`
	SecretsStoreFile                string      `json:"secrets_store_file"`
}
type PullConfigs interface {
	pullOpensearchConfigs() (map[string]*ConfigKeys, error)
	pullPGConfigs() (map[string]*ConfigKeys, error)
	pullAutomateConfigs() (map[string]*dc.AutomateConfig, error)
	pullChefServerConfigs() (map[string]*dc.AutomateConfig, error)
	fetchInfraConfig() (*ExistingInfraConfigToml, error)
	generateInfraConfig() (*ExistingInfraConfigToml, error)
	fetchAwsConfig() (*AwsConfigToml, error)
	generateAwsConfig() (*AwsConfigToml, error)
	getExceptionIps() []string
	setExceptionIps(ips []string)
}

type PullConfigsImpl struct {
	infra        *AutomateHAInfraDetails
	sshUtil      SSHUtil
	exceptionIps []string
}

func NewPullConfigs(infra *AutomateHAInfraDetails, sshUtil SSHUtil) PullConfigs {
	return &PullConfigsImpl{
		infra:   infra,
		sshUtil: sshUtil,
	}
}

func (p *PullConfigsImpl) getExceptionIps() []string {
	return p.exceptionIps
}

func (p *PullConfigsImpl) setExceptionIps(ips []string) {
	p.exceptionIps = ips
}

func (p *PullConfigsImpl) pullOpensearchConfigs() (map[string]*ConfigKeys, error) {
	ipConfigMap := make(map[string]*ConfigKeys)
	for _, ip := range p.infra.Outputs.OpensearchPrivateIps.Value {
		if stringutils.SliceContains(p.exceptionIps, ip) {
			continue
		}
		p.sshUtil.getSSHConfig().hostIP = ip
		scriptCommands := fmt.Sprintf(GET_CONFIG, opensearch_const)
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			return nil, err
		}
		var src OpensearchConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return nil, err
		}
		ipConfigMap[ip] = &ConfigKeys{
			rootCA:     src.TLS.RootCA,
			privateKey: src.TLS.SslKey,
			publicKey:  src.TLS.SslCert,
			adminCert:  src.TLS.AdminCert,
			adminKey:   src.TLS.AdminKey,
		}
	}
	return ipConfigMap, nil
}

func (p *PullConfigsImpl) pullPGConfigs() (map[string]*ConfigKeys, error) {
	ipConfigMap := make(map[string]*ConfigKeys)
	for _, ip := range p.infra.Outputs.PostgresqlPrivateIps.Value {
		if stringutils.SliceContains(p.exceptionIps, ip) {
			continue
		}
		p.sshUtil.getSSHConfig().hostIP = ip
		scriptCommands := fmt.Sprintf(GET_CONFIG, postgresql)
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
		if err != nil {
			return nil, err
		}
		var src PostgresqlConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return nil, err
		}
		ipConfigMap[ip] = &ConfigKeys{
			rootCA:     src.Ssl.IssuerCert,
			privateKey: src.Ssl.SslKey,
			publicKey:  src.Ssl.SslCert,
		}
	}
	return ipConfigMap, nil
}

func (p *PullConfigsImpl) pullAutomateConfigs() (map[string]*dc.AutomateConfig, error) {
	ipConfigMap := make(map[string]*dc.AutomateConfig)
	for _, ip := range p.infra.Outputs.AutomatePrivateIps.Value {
		if stringutils.SliceContains(p.exceptionIps, ip) {
			continue
		}
		p.sshUtil.getSSHConfig().hostIP = ip
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(fmt.Sprintf(GET_FRONTEND_CONFIG, ""), true)
		if err != nil {
			return nil, err
		}
		var src dc.AutomateConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return nil, err
		}
		ipConfigMap[ip] = &src
	}
	return ipConfigMap, nil

}

func (p *PullConfigsImpl) pullChefServerConfigs() (map[string]*dc.AutomateConfig, error) {
	ipConfigMap := make(map[string]*dc.AutomateConfig)
	for _, ip := range p.infra.Outputs.ChefServerPrivateIps.Value {
		if stringutils.SliceContains(p.exceptionIps, ip) {
			continue
		}
		p.sshUtil.getSSHConfig().hostIP = ip
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(fmt.Sprintf(GET_FRONTEND_CONFIG, ""), true)
		if err != nil {
			return nil, err
		}
		var src dc.AutomateConfig
		if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
			return nil, err
		}
		ipConfigMap[ip] = &src
	}
	return ipConfigMap, nil
}

func (p *PullConfigsImpl) fetchInfraConfig() (*ExistingInfraConfigToml, error) {
	sharedConfigToml, err := getExistingHAConfig()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch HA config")
	}
	a2ConfigMap, err := p.pullAutomateConfigs()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch Automate config")
	}
	csConfigMap, err := p.pullChefServerConfigs()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch Chef Server config")
	}
	// checking onprem with managed or self managed services
	logrus.Debug(sharedConfigToml.ExternalDB.Database.Type)
	if len(strings.TrimSpace(sharedConfigToml.ExternalDB.Database.Type)) < 1 {
		osConfigMap, err := p.pullOpensearchConfigs()
		if err != nil {
			return nil, status.Wrap(err, status.ConfigError, "unable to fetch Opensearch config")
		}
		pgConfigMap, err := p.pullPGConfigs()
		if err != nil {
			return nil, status.Wrap(err, status.ConfigError, "unable to fetch Postgresql config")
		}

		// Build CertsByIP for Opensearch
		var osCerts []CertByIP
		for key, ele := range osConfigMap {
			nodeDn, err := getDistinguishedNameFromKey(ele.publicKey)
			if err != nil {
				writer.Fail(err.Error())
			}
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.privateKey,
				PublicKey:  ele.publicKey,
				NodesDn:    fmt.Sprintf("%v", nodeDn),
			}
			osCerts = append(osCerts, certByIP)
		}
		sharedConfigToml.Opensearch.Config.CertsByIP = osCerts

		if osRootCA := getOSORPGRootCA(osConfigMap); len(osRootCA) > 0 {
			sharedConfigToml.Opensearch.Config.RootCA = osRootCA
		}
		osAdminCert, osAdminKey := getOSAdminCertAndAdminKey(osConfigMap)
		if len(osAdminCert) > 0 {
			sharedConfigToml.Opensearch.Config.AdminCert = osAdminCert
		}
		if len(osAdminKey) > 0 {
			sharedConfigToml.Opensearch.Config.AdminKey = osAdminKey
		}
		adminDn, err := getDistinguishedNameFromKey(sharedConfigToml.Opensearch.Config.AdminCert)
		if err != nil {
			writer.Fail(err.Error())
		}
		sharedConfigToml.Opensearch.Config.AdminDn = fmt.Sprintf("%v", adminDn)
		sharedConfigToml.Opensearch.Config.EnableCustomCerts = true

		// Build CertsByIP for Postgresql
		var pgCerts []CertByIP
		for key, ele := range pgConfigMap {
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.privateKey,
				PublicKey:  ele.publicKey,
			}
			pgCerts = append(pgCerts, certByIP)
		}
		sharedConfigToml.Postgresql.Config.CertsByIP = pgCerts

		if pgRootCA := getOSORPGRootCA(pgConfigMap); len(pgRootCA) > 0 {
			sharedConfigToml.Postgresql.Config.RootCA = pgRootCA
		}
		sharedConfigToml.Postgresql.Config.EnableCustomCerts = true
	}

	// Build CertsByIP for Automate
	var a2Certs []CertByIP
	for key, ele := range a2ConfigMap {
		certByIP := CertByIP{
			IP:         key,
			PrivateKey: ele.Global.V1.FrontendTls[0].Key,
			PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
		}
		a2Certs = append(a2Certs, certByIP)
	}
	sharedConfigToml.Automate.Config.CertsByIP = a2Certs

	sharedConfigToml.Automate.Config.EnableCustomCerts = true

	// Build CertsByIP for ChefServer
	var csCerts []CertByIP
	for key, ele := range csConfigMap {
		if len(ele.Global.V1.FrontendTls) > 0 {
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.Global.V1.FrontendTls[0].Key,
				PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
			}
			csCerts = append(csCerts, certByIP)
		}
	}
	sharedConfigToml.ChefServer.Config.CertsByIP = csCerts

	if csRootCA := getRootCAFromCS(csConfigMap); len(csRootCA) > 0 {
		sharedConfigToml.Automate.Config.RootCA = csRootCA
	}
	sharedConfigToml.ChefServer.Config.EnableCustomCerts = true

	objectStorageConfig := getS3BackConfig(a2ConfigMap)
	if len(objectStorageConfig.accessKey) > 0 {
		sharedConfigToml.ObjectStorage.Config.AccessKey = objectStorageConfig.accessKey
	}
	if len(objectStorageConfig.secrectKey) > 0 {
		sharedConfigToml.ObjectStorage.Config.SecretKey = objectStorageConfig.secrectKey

	}
	if len(objectStorageConfig.bucketName) > 0 {
		sharedConfigToml.ObjectStorage.Config.BucketName = objectStorageConfig.bucketName
	}

	if len(objectStorageConfig.endpoint) > 0 {
		sharedConfigToml.ObjectStorage.Config.Endpoint = objectStorageConfig.endpoint
	}

	return sharedConfigToml, nil
}

func (p *PullConfigsImpl) generateInfraConfig() (*ExistingInfraConfigToml, error) {
	sharedConfigToml, err := p.fetchInfraConfig()
	if err != nil {
		return nil, err
	}

	shardConfig, err := mtoml.Marshal(sharedConfigToml)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to marshal config to file")
	}
	err = ioutil.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"), shardConfig, 0644) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to write config toml to file")
	}

	return sharedConfigToml, nil
}

func (p *PullConfigsImpl) fetchAwsConfig() (*AwsConfigToml, error) {
	sharedConfigToml, err := getAwsHAConfig()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch HA config")
	}
	archBytes, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) // nosemgrep
	if err != nil {
		writer.Errorf("%s", err.Error())
		return nil, err
	}
	var arch = strings.Trim(string(archBytes), "\n")
	sharedConfigToml.Architecture.ConfigInitials.Architecture = arch

	a2ConfigMap, err := p.pullAutomateConfigs()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch Automate config")
	}
	csConfigMap, err := p.pullChefServerConfigs()
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to fetch Chef Server config")
	}
	// checking AWS with managed services or Non managed services
	logrus.Debug(sharedConfigToml.Aws.Config.SetupManagedServices)
	if !isManagedServicesOn() {
		osConfigMap, err := p.pullOpensearchConfigs()
		if err != nil {
			return nil, status.Wrap(err, status.ConfigError, "unable to fetch Opensearch config")
		}
		pgConfigMap, err := p.pullPGConfigs()
		if err != nil {
			return nil, status.Wrap(err, status.ConfigError, "unable to fetch Postgresql config")
		}

		// Build CertsByIP for Opensearch
		var osCerts []CertByIP
		for key, ele := range osConfigMap {
			nodeDn, err := getDistinguishedNameFromKey(ele.publicKey)
			if err != nil {
				writer.Fail(err.Error())
			}
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.privateKey,
				PublicKey:  ele.publicKey,
				NodesDn:    fmt.Sprintf("%v", nodeDn),
			}
			osCerts = append(osCerts, certByIP)
		}
		sharedConfigToml.Opensearch.Config.CertsByIP = osCerts

		if osRootCA := getOSORPGRootCA(osConfigMap); len(osRootCA) > 0 {
			sharedConfigToml.Opensearch.Config.RootCA = osRootCA
		}
		osAdminCert, osAdminKey := getOSAdminCertAndAdminKey(osConfigMap)
		if len(osAdminCert) > 0 {
			sharedConfigToml.Opensearch.Config.AdminCert = osAdminCert
		}
		if len(osAdminKey) > 0 {
			sharedConfigToml.Opensearch.Config.AdminKey = osAdminKey
		}

		osPrivKey, osPubKey := getPrivateKeyAndPublicKeyFromBE(osConfigMap)
		if len(osPrivKey) > 0 {
			sharedConfigToml.Opensearch.Config.PrivateKey = osPrivKey
		}
		if len(osPubKey) > 0 {
			sharedConfigToml.Opensearch.Config.PublicKey = osPubKey
		}
		nodeDn, err := getDistinguishedNameFromKey(sharedConfigToml.Opensearch.Config.PublicKey)
		if err != nil {
			writer.Fail(err.Error())
		}
		adminDn, err := getDistinguishedNameFromKey(sharedConfigToml.Opensearch.Config.AdminCert)
		if err != nil {
			writer.Fail(err.Error())
		}
		sharedConfigToml.Opensearch.Config.NodesDn = fmt.Sprintf("%v", nodeDn)
		sharedConfigToml.Opensearch.Config.AdminDn = fmt.Sprintf("%v", adminDn)
		sharedConfigToml.Opensearch.Config.EnableCustomCerts = true

		// Build CertsByIP for Postgresql
		var pgCerts []CertByIP
		for key, ele := range pgConfigMap {
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.privateKey,
				PublicKey:  ele.publicKey,
			}
			pgCerts = append(pgCerts, certByIP)
		}
		sharedConfigToml.Postgresql.Config.CertsByIP = pgCerts

		if pgRootCA := getOSORPGRootCA(pgConfigMap); len(pgRootCA) > 0 {
			sharedConfigToml.Postgresql.Config.RootCA = pgRootCA
		}
		pgPrivKey, pgPubKey := getPrivateKeyAndPublicKeyFromBE(pgConfigMap)
		if len(pgPrivKey) > 0 {
			sharedConfigToml.Postgresql.Config.PrivateKey = pgPrivKey
		}
		if len(pgPubKey) > 0 {
			sharedConfigToml.Postgresql.Config.PublicKey = pgPubKey
		}
		sharedConfigToml.Postgresql.Config.EnableCustomCerts = true
	}

	// Build CertsByIP for Automate
	var a2Certs []CertByIP
	for key, ele := range a2ConfigMap {
		certByIP := CertByIP{
			IP:         key,
			PrivateKey: ele.Global.V1.FrontendTls[0].Key,
			PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
		}
		a2Certs = append(a2Certs, certByIP)
	}
	sharedConfigToml.Automate.Config.CertsByIP = a2Certs

	a2Fqdn := getA2fqdn(a2ConfigMap)
	if len(a2Fqdn) > 0 {
		sharedConfigToml.Automate.Config.Fqdn = a2Fqdn
	}
	if a2PrivKey := getPrivateKeyFromFE(a2ConfigMap); len(a2PrivKey) > 0 {
		sharedConfigToml.Automate.Config.PrivateKey = a2PrivKey
	}
	if a2PubKey := getPublicKeyFromFE(a2ConfigMap); len(a2PubKey) > 0 {
		sharedConfigToml.Automate.Config.PublicKey = a2PubKey
	}
	sharedConfigToml.Automate.Config.EnableCustomCerts = true

	// Build CertsByIP for ChefServer
	var csCerts []CertByIP
	for key, ele := range csConfigMap {
		if len(ele.Global.V1.FrontendTls) > 0 {
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.Global.V1.FrontendTls[0].Key,
				PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
			}
			csCerts = append(csCerts, certByIP)
		}
	}
	sharedConfigToml.ChefServer.Config.CertsByIP = csCerts

	if csRootCA := getRootCAFromCS(csConfigMap); len(csRootCA) > 0 {
		sharedConfigToml.Automate.Config.RootCA = csRootCA
	}
	if csPrivKey := getPrivateKeyFromFE(csConfigMap); len(csPrivKey) > 0 {
		sharedConfigToml.ChefServer.Config.PrivateKey = csPrivKey
	}
	if csPubKey := getPublicKeyFromFE(csConfigMap); len(csPubKey) > 0 {
		sharedConfigToml.ChefServer.Config.PublicKey = csPubKey
	}
	sharedConfigToml.ChefServer.Config.EnableCustomCerts = true

	objStorageConfig := getOpenSearchObjectStorageConfig(a2ConfigMap)
	if len(objStorageConfig.bucketName) > 0 {
		sharedConfigToml.Architecture.ConfigInitials.S3BucketName = objStorageConfig.bucketName
	}
	if len(objStorageConfig.RoleArn) > 0 {
		sharedConfigToml.Aws.Config.AwsOsSnapshotRoleArn = objStorageConfig.RoleArn
	}
	if len(objStorageConfig.accessKey) > 0 {
		sharedConfigToml.Aws.Config.OsUserAccessKeyId = objStorageConfig.accessKey
	}
	if len(objStorageConfig.secrectKey) > 0 {
		sharedConfigToml.Aws.Config.OsUserAccessKeySecret = objStorageConfig.secrectKey
	}

	return sharedConfigToml, nil
}

func (p *PullConfigsImpl) generateAwsConfig() (*AwsConfigToml, error) {
	sharedConfigToml, err := p.fetchAwsConfig()
	if err != nil {
		return nil, err
	}

	shardConfig, err := mtoml.Marshal(sharedConfigToml)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to marshal config to file")
	}
	err = ioutil.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"), shardConfig, 0644) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "unable to write config toml to file")
	}

	return sharedConfigToml, nil
}

func getExistingHAConfig() (*ExistingInfraConfigToml, error) {
	if checkSharedConfigFile() {
		sharedConfigToml, err := getExistingInfraConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return nil, err
		}
		return sharedConfigToml, nil
	} else {
		jsonString := convTfvarToJson(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "terraform.tfvars"))
		tfvarConfig, err := getJsonFromTerraformTfVarsFile(jsonString)
		if err != nil {
			return nil, err
		}
		return getExistingHAConfigFromTFVars(tfvarConfig)
	}
}

func getAwsHAConfig() (*AwsConfigToml, error) {
	if checkSharedConfigFile() {
		sharedConfigToml, err := getAwsConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return nil, err
		}
		return sharedConfigToml, nil
	} else {
		jsonString := convTfvarToJson(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "terraform.tfvars"))
		tfvarConfig, err := getJsonFromTerraformTfVarsFile(jsonString)
		if err != nil {
			return nil, err
		}
		AwsConfigJsonString := convTfvarToJson(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "aws.auto.tfvars"))
		awsAutoTfvarConfig, err := getJsonFromTerraformAwsAutoTfVarsFile(AwsConfigJsonString)
		if err != nil {
			return nil, err
		}
		return getAwsHAConfigFromTFVars(tfvarConfig, awsAutoTfvarConfig)
	}
}

func getExistingHAConfigFromTFVars(tfvarConfig *HATfvars) (*ExistingInfraConfigToml, error) {
	sharedConfigToml := &ExistingInfraConfigToml{}
	sharedConfigToml.Architecture.ConfigInitials.Architecture = "existing_nodes"
	if len(strings.TrimSpace(tfvarConfig.SecretsKeyFile)) < 1 {
		sharedConfigToml.Architecture.ConfigInitials.SecretsKeyFile = filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "secrets.key")
	} else {
		sharedConfigToml.Architecture.ConfigInitials.SecretsKeyFile = tfvarConfig.SecretsKeyFile
	}

	if len(strings.TrimSpace(tfvarConfig.SecretsStoreFile)) < 1 {
		sharedConfigToml.Architecture.ConfigInitials.SecretsStoreFile = filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "secrets.json")
	} else {
		sharedConfigToml.Architecture.ConfigInitials.SecretsStoreFile = tfvarConfig.SecretsStoreFile
	}

	if strings.EqualFold(strings.TrimSpace(tfvarConfig.BackupConfigS3), "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "object_storage"
	}
	if strings.EqualFold(strings.TrimSpace(tfvarConfig.BackupConfigEFS), "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "file_system"
	}
	if tfvarConfig.TeamsPort > 0 {
		sharedConfigToml.Automate.Config.TeamsPort = strconv.Itoa(tfvarConfig.TeamsPort)
	}
	sharedConfigToml.Architecture.ConfigInitials.BackupMount = strings.TrimSpace(tfvarConfig.NfsMountPath)
	sharedConfigToml.Architecture.ConfigInitials.HabitatUIDGid = strings.TrimSpace(tfvarConfig.HabitatUidGid)
	sharedConfigToml.Architecture.ConfigInitials.SSHKeyFile = strings.TrimSpace(tfvarConfig.SshKeyFile)
	sharedConfigToml.Architecture.ConfigInitials.SSHPort = strings.TrimSpace(tfvarConfig.SshPort)
	sharedConfigToml.Architecture.ConfigInitials.SSHUser = strings.TrimSpace(tfvarConfig.SshUser)
	sharedConfigToml.Architecture.ConfigInitials.WorkspacePath = AUTOMATE_HA_WORKSPACE_DIR
	sharedConfigToml.Automate.Config.Fqdn = strings.TrimSpace(tfvarConfig.AutomateFqdn)
	sharedConfigToml.Automate.Config.InstanceCount = strconv.Itoa(tfvarConfig.AutomateInstanceCount)
	sharedConfigToml.Automate.Config.ConfigFile = strings.TrimSpace(tfvarConfig.AutomateConfigFile)
	sharedConfigToml.Automate.Config.EnableCustomCerts = tfvarConfig.AutomateCustomCertsEnabled
	sharedConfigToml.Automate.Config.AdminPassword = strings.TrimSpace(tfvarConfig.AutomateAdminPassword)
	sharedConfigToml.ChefServer.Config.EnableCustomCerts = tfvarConfig.ChefServerCustomCertsEnabled
	sharedConfigToml.ChefServer.Config.InstanceCount = strconv.Itoa(tfvarConfig.ChefServerInstanceCount)
	sharedConfigToml.Postgresql.Config.EnableCustomCerts = tfvarConfig.PostgresqlCustomCertsEnabled
	sharedConfigToml.Postgresql.Config.InstanceCount = strconv.Itoa(tfvarConfig.PostgresqlInstanceCount)
	sharedConfigToml.Opensearch.Config.EnableCustomCerts = tfvarConfig.OpensearchCustomCertsEnabled
	sharedConfigToml.Opensearch.Config.InstanceCount = strconv.Itoa(tfvarConfig.OpensearchInstanceCount)
	sharedConfigToml.Opensearch.Config.AdminDn = strings.TrimSpace(tfvarConfig.OpensearchAdminDn)
	sharedConfigToml.Opensearch.Config.NodesDn = strings.TrimSpace(tfvarConfig.OpensearchNodesDn)
	sharedConfigToml.ExistingInfra.Config.AutomatePrivateIps = tfvarConfig.ExistingAutomatePrivateIps
	sharedConfigToml.ExistingInfra.Config.ChefServerPrivateIps = tfvarConfig.ExistingChefServerPrivateIps
	sharedConfigToml.ExistingInfra.Config.PostgresqlPrivateIps = tfvarConfig.ExistingPostgresqlPrivateIps
	sharedConfigToml.ExistingInfra.Config.OpensearchPrivateIps = tfvarConfig.ExistingOpensearchPrivateIps
	sharedConfigToml.ObjectStorage.Config.AccessKey = strings.TrimSpace(tfvarConfig.AccessKey)
	sharedConfigToml.ObjectStorage.Config.SecretKey = strings.TrimSpace(tfvarConfig.SecretKey)
	sharedConfigToml.ObjectStorage.Config.BucketName = strings.TrimSpace(tfvarConfig.BucketName)
	sharedConfigToml.ObjectStorage.Config.Endpoint = strings.TrimSpace(tfvarConfig.Endpoint)
	sharedConfigToml.ObjectStorage.Config.Region = strings.TrimSpace(tfvarConfig.Region)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchInstanceURL = strings.TrimSpace(tfvarConfig.ManagedOpensearchDomainUrl)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchDomainName = strings.TrimSpace(tfvarConfig.ManagedOpensearchDomainName)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchCertificate = strings.TrimSpace(tfvarConfig.ManagedOpensearchCertificate)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchRootCert = strings.TrimSpace(tfvarConfig.OpensearchRootCert)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchSuperUserName = strings.TrimSpace(tfvarConfig.ManagedOpensearchUsername)
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword = strings.TrimSpace(tfvarConfig.ManagedOpensearchUserPassword)
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.AwsOsSnapshotRoleArn = strings.TrimSpace(tfvarConfig.AwsOsSnapshotRoleArn)
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeyId = strings.TrimSpace(tfvarConfig.OsSnapshotUserAccessKeyId)
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeySecret = strings.TrimSpace(tfvarConfig.OsSnapshotUserAccessKeySecret)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLCertificate = strings.TrimSpace(tfvarConfig.ManagedRdsPostgresqlCertificate)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName = strings.TrimSpace(tfvarConfig.ManagedRdsDbuserUsername)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword = strings.TrimSpace(tfvarConfig.ManagedRdsDbuserPassword)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL = strings.TrimSpace(tfvarConfig.ManagedRdsInstanceUrl)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLRootCert = strings.TrimSpace(tfvarConfig.PostgresqlRootCert)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName = strings.TrimSpace(tfvarConfig.ManagedRdsSuperuserUsername)
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword = strings.TrimSpace(tfvarConfig.ManagedRdsSuperuserPassword)
	setupManagedServices := strings.TrimSpace(tfvarConfig.SetupManagedServices)
	setupSelfManagedServices := strings.TrimSpace(tfvarConfig.SetupSelfManagedServices)
	if strings.EqualFold(setupManagedServices, "true") && strings.EqualFold(setupSelfManagedServices, "true") {
		sharedConfigToml.ExternalDB.Database.Type = "self-managed"
	}
	if strings.EqualFold(setupManagedServices, "true") && strings.EqualFold(setupSelfManagedServices, "false") {
		sharedConfigToml.ExternalDB.Database.Type = "aws"
	}
	return sharedConfigToml, nil
}

func getAwsHAConfigFromTFVars(tfvarConfig *HATfvars, awsAutoTfvarConfig *HAAwsAutoTfvars) (*AwsConfigToml, error) {
	sharedConfigToml := &AwsConfigToml{}
	sharedConfigToml.Architecture.ConfigInitials.Architecture = "deployment"

	if len(strings.TrimSpace(tfvarConfig.SecretsKeyFile)) < 1 {
		sharedConfigToml.Architecture.ConfigInitials.SecretsKeyFile = filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "secrets.key")
	} else {
		sharedConfigToml.Architecture.ConfigInitials.SecretsKeyFile = tfvarConfig.SecretsKeyFile
	}

	if len(strings.TrimSpace(tfvarConfig.SecretsStoreFile)) < 1 {
		sharedConfigToml.Architecture.ConfigInitials.SecretsStoreFile = filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "secrets.json")
	} else {
		sharedConfigToml.Architecture.ConfigInitials.SecretsStoreFile = tfvarConfig.SecretsStoreFile
	}

	if strings.EqualFold(strings.TrimSpace(awsAutoTfvarConfig.BackupConfigS3), "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "s3"
	}

	if strings.EqualFold(strings.TrimSpace(awsAutoTfvarConfig.BackupConfigEFS), "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "efs"
	}

	if tfvarConfig.TeamsPort > 0 {
		sharedConfigToml.Automate.Config.TeamsPort = strconv.Itoa(tfvarConfig.TeamsPort)
	}

	sharedConfigToml.Architecture.ConfigInitials.BackupMount = strings.TrimSpace(awsAutoTfvarConfig.NfsMountPath)
	sharedConfigToml.Architecture.ConfigInitials.HabitatUIDGid = strings.TrimSpace(awsAutoTfvarConfig.HabitatUidGid)
	sharedConfigToml.Architecture.ConfigInitials.Architecture = strings.TrimSpace(awsAutoTfvarConfig.Architecture)
	sharedConfigToml.Architecture.ConfigInitials.SSHKeyFile = strings.TrimSpace(awsAutoTfvarConfig.AwsSshKeyFile)
	sharedConfigToml.Architecture.ConfigInitials.SSHPort = strings.TrimSpace(awsAutoTfvarConfig.SshPort)
	sharedConfigToml.Architecture.ConfigInitials.SSHUser = strings.TrimSpace(awsAutoTfvarConfig.SshUser)
	sharedConfigToml.Architecture.ConfigInitials.WorkspacePath = AUTOMATE_HA_WORKSPACE_DIR
	sharedConfigToml.Automate.Config.InstanceCount = strconv.Itoa(awsAutoTfvarConfig.AutomateInstanceCount)
	sharedConfigToml.Automate.Config.ConfigFile = strings.TrimSpace(awsAutoTfvarConfig.AutomateConfigFile)
	sharedConfigToml.Automate.Config.EnableCustomCerts = awsAutoTfvarConfig.AutomateCustomCertsEnabled
	sharedConfigToml.Automate.Config.AdminPassword = strings.TrimSpace(awsAutoTfvarConfig.AutomateAdminPassword)
	sharedConfigToml.ChefServer.Config.EnableCustomCerts = awsAutoTfvarConfig.ChefServerCustomCertsEnabled
	sharedConfigToml.ChefServer.Config.InstanceCount = strconv.Itoa(awsAutoTfvarConfig.ChefServerInstanceCount)
	sharedConfigToml.Postgresql.Config.EnableCustomCerts = awsAutoTfvarConfig.PostgresqlCustomCertsEnabled
	sharedConfigToml.Postgresql.Config.InstanceCount = strconv.Itoa(awsAutoTfvarConfig.PostgresqlInstanceCount)
	sharedConfigToml.Opensearch.Config.EnableCustomCerts = awsAutoTfvarConfig.OpensearchCustomCertsEnabled
	sharedConfigToml.Opensearch.Config.InstanceCount = strconv.Itoa(awsAutoTfvarConfig.OpensearchInstanceCount)
	sharedConfigToml.Opensearch.Config.AdminDn = strings.TrimSpace(awsAutoTfvarConfig.OpensearchAdminDn)
	sharedConfigToml.Opensearch.Config.NodesDn = strings.TrimSpace(awsAutoTfvarConfig.OpensearchNodesDn)
	sharedConfigToml.Aws.Config.Profile = strings.TrimSpace(awsAutoTfvarConfig.AwsProfile)
	sharedConfigToml.Aws.Config.Region = strings.TrimSpace(awsAutoTfvarConfig.AwsRegion)
	sharedConfigToml.Aws.Config.AwsVpcId = strings.TrimSpace(awsAutoTfvarConfig.AwsVpcId)
	sharedConfigToml.Aws.Config.AmiID = strings.TrimSpace(awsAutoTfvarConfig.AmiID)
	sharedConfigToml.Aws.Config.SSHKeyPairName = strings.TrimSpace(awsAutoTfvarConfig.SshKeyFileName)
	sharedConfigToml.Aws.Config.AwsCidrBlockAddr = strings.TrimSpace(awsAutoTfvarConfig.AwsCidrBlockAddr)
	sharedConfigToml.Aws.Config.PrivateCustomSubnets = awsAutoTfvarConfig.PrivateCustomSubnets
	sharedConfigToml.Aws.Config.PublicCustomSubnets = awsAutoTfvarConfig.PublicCustomSubnets
	sharedConfigToml.Aws.Config.SSHKeyPairName = strings.TrimSpace(awsAutoTfvarConfig.SshKeyFileName)
	sharedConfigToml.Aws.Config.AutomateLbCertificateArn = strings.TrimSpace(awsAutoTfvarConfig.AutomateLbCertificateArn)
	sharedConfigToml.Aws.Config.ChefServerLbCertificateArn = strings.TrimSpace(awsAutoTfvarConfig.ChefServerLbCertificateArn)
	sharedConfigToml.Aws.Config.OpensearchDomainUrl = strings.TrimSpace(awsAutoTfvarConfig.ManagedOpensearchDomainUrl)
	sharedConfigToml.Aws.Config.OpensearchDomainName = strings.TrimSpace(awsAutoTfvarConfig.ManagedOpensearchDomainName)
	sharedConfigToml.Aws.Config.OpensearchCertificate = strings.TrimSpace(awsAutoTfvarConfig.ManagedOpensearchCertificate)
	sharedConfigToml.Aws.Config.OpensearchUsername = strings.TrimSpace(awsAutoTfvarConfig.ManagedOpensearchUsername)
	sharedConfigToml.Aws.Config.OpensearchUserPassword = strings.TrimSpace(awsAutoTfvarConfig.ManagedOpensearchUserPassword)
	sharedConfigToml.Aws.Config.RDSCertificate = strings.TrimSpace(awsAutoTfvarConfig.AwsManagedRdsPostgresqlCertificate)
	sharedConfigToml.Aws.Config.RDSDBUserName = strings.TrimSpace(awsAutoTfvarConfig.ManagedRdsDbuserUsername)
	sharedConfigToml.Aws.Config.RDSDBUserPassword = strings.TrimSpace(awsAutoTfvarConfig.ManagedRdsDbuserPassword)
	sharedConfigToml.Aws.Config.RDSInstanceUrl = strings.TrimSpace(awsAutoTfvarConfig.ManagedRdsInstanceUrl)
	sharedConfigToml.Aws.Config.RDSSuperUserName = strings.TrimSpace(awsAutoTfvarConfig.ManagedRdsSuperuserUsername)
	sharedConfigToml.Aws.Config.RDSSuperUserPassword = strings.TrimSpace(awsAutoTfvarConfig.ManagedRdsSuperuserPassword)
	sharedConfigToml.Aws.Config.LBAccessLogs = strings.TrimSpace(awsAutoTfvarConfig.LBAccessLogs)
	sharedConfigToml.Aws.Config.DeleteOnTermination, _ = strconv.ParseBool(awsAutoTfvarConfig.DeleteOnTermination)
	sharedConfigToml.Aws.Config.AutomateServerInstanceType = strings.TrimSpace(awsAutoTfvarConfig.AutomateServerInstanceType)
	sharedConfigToml.Aws.Config.AutomateEbsVolumeIops = strings.TrimSpace(awsAutoTfvarConfig.AutomateEbsVolumeIops)
	sharedConfigToml.Aws.Config.AutomateEbsVolumeSize = strings.TrimSpace(awsAutoTfvarConfig.AutomateEbsVolumeSize)
	sharedConfigToml.Aws.Config.AutomateEbsVolumeType = strings.TrimSpace(awsAutoTfvarConfig.AutomateEbsVolumeType)
	sharedConfigToml.Aws.Config.ChefServerInstanceType = strings.TrimSpace(awsAutoTfvarConfig.ChefServerInstanceType)
	sharedConfigToml.Aws.Config.ChefEbsVolumeSize = strings.TrimSpace(awsAutoTfvarConfig.ChefEbsVolumeSize)
	sharedConfigToml.Aws.Config.ChefEbsVolumeType = strings.TrimSpace(awsAutoTfvarConfig.ChefEbsVolumeType)
	sharedConfigToml.Aws.Config.OpensearchServerInstanceType = strings.TrimSpace(awsAutoTfvarConfig.OpensearchServerInstanceType)
	sharedConfigToml.Aws.Config.OpensearchEbsVolumeIops = strings.TrimSpace(awsAutoTfvarConfig.OpensearchEbsVolumeIops)
	sharedConfigToml.Aws.Config.OpensearchEbsVolumeSize = strings.TrimSpace(awsAutoTfvarConfig.OpensearchEbsVolumeSize)
	sharedConfigToml.Aws.Config.OpensearchEbsVolumeType = strings.TrimSpace(awsAutoTfvarConfig.OpensearchEbsVolumeType)
	sharedConfigToml.Aws.Config.PostgresqlServerInstanceType = strings.TrimSpace(awsAutoTfvarConfig.PostgresqlServerInstanceType)
	sharedConfigToml.Aws.Config.PostgresqlEbsVolumeIops = strings.TrimSpace(awsAutoTfvarConfig.PostgresqlEbsVolumeIops)
	sharedConfigToml.Aws.Config.PostgresqlEbsVolumeSize = strings.TrimSpace(awsAutoTfvarConfig.PostgresqlEbsVolumeSize)
	sharedConfigToml.Aws.Config.PostgresqlEbsVolumeType = strings.TrimSpace(awsAutoTfvarConfig.PostgresqlEbsVolumeType)
	sharedConfigToml.Aws.Config.SetupManagedServices = awsAutoTfvarConfig.AwsSetupManagedServices
	sharedConfigToml.Aws.Config.ChefEbsVolumeIops, _ = getTheValueFromA2HARB("chef_ebs_volume_iops")
	return sharedConfigToml, nil
}

func getTheValueFromA2HARB(key string) (string, error) {
	wordCountCmd := `HAB_LICENSE=accept-no-persist hab pkg exec core/grep grep %s %s | wc -l`
	WordCountF := fmt.Sprintf(wordCountCmd, key, AUTOMATE_HA_WORKSPACE_A2HARB_FILE)
	output, err := exec.Command("/bin/sh", "-c", WordCountF).Output()
	if err != nil {
		return "", err
	}
	value := string(output)
	count, _ := strconv.Atoi(value)
	if count > 1 {
		logrus.Debug("The Key has more than one value it cannot be executed")
		return "", nil
	} else {
		GrepCmd := `HAB_LICENSE=accept-no-persist hab pkg exec core/grep grep %s %s | hab pkg exec core/gawk gawk '{print $2}'`
		GrepCmdF := fmt.Sprintf(GrepCmd, key, AUTOMATE_HA_WORKSPACE_A2HARB_FILE)
		output, err := exec.Command("/bin/sh", "-c", GrepCmdF).Output()
		if err != nil {
			return "", err
		}
		value := string(output)
		read_line := strings.TrimSuffix(value, "\n")
		KeyV, err := strconv.Unquote(read_line)
		if err != nil {
			return "", err
		}
		return strings.TrimSpace(KeyV), nil
	}
}

func getOSORPGRootCA(config map[string]*ConfigKeys) string {
	for _, ele := range config {
		if len(strings.TrimSpace(ele.rootCA)) > 0 {
			return strings.TrimSpace(ele.rootCA)
		}
	}
	return ""
}

func getRootCAFromCS(config map[string]*dc.AutomateConfig) string {
	if config == nil {
		return ""
	}
	for _, ele := range config {
		if ele.Global.V1.External != nil && ele.Global.V1.External.Automate != nil && ele.Global.V1.External.Automate.Ssl != nil && ele.Global.V1.External.Automate.Ssl.RootCert != nil {
			return ele.GetGlobal().V1.External.Automate.Ssl.RootCert.Value
		}
	}
	return ""
}

func getPrivateKeyFromFE(config map[string]*dc.AutomateConfig) string {
	if config == nil {
		return ""
	}
	for _, ele := range config {
		if ele.Global.V1.FrontendTls[0] != nil && ele.Global.V1.FrontendTls[0].Key != "" {
			return ele.GetGlobal().V1.FrontendTls[0].Key
		}
	}
	return ""
}

func getPublicKeyFromFE(config map[string]*dc.AutomateConfig) string {
	if config == nil {
		return ""
	}
	for _, ele := range config {
		if ele.Global.V1.FrontendTls[0] != nil && ele.Global.V1.FrontendTls[0].Cert != "" {
			return ele.GetGlobal().V1.FrontendTls[0].Cert
		}
	}
	return ""
}

func getOSAdminCertAndAdminKey(config map[string]*ConfigKeys) (string, string) {
	for _, ele := range config {
		if len(strings.TrimSpace(ele.adminCert)) > 0 && len(strings.TrimSpace(ele.adminKey)) > 0 {
			return strings.TrimSpace(ele.adminCert), strings.TrimSpace(ele.adminKey)
		} else if len(strings.TrimSpace(ele.adminCert)) > 0 {
			return strings.TrimSpace(ele.adminCert), ""
		} else if len(strings.TrimSpace(ele.adminKey)) > 0 {
			return "", strings.TrimSpace(ele.adminKey)
		}
	}
	return "", ""
}

func getPrivateKeyAndPublicKeyFromBE(config map[string]*ConfigKeys) (string, string) {
	for _, ele := range config {
		if len(strings.TrimSpace(ele.privateKey)) > 0 && len(strings.TrimSpace(ele.publicKey)) > 0 {
			return strings.TrimSpace(ele.privateKey), strings.TrimSpace(ele.publicKey)
		} else if len(strings.TrimSpace(ele.privateKey)) > 0 {
			return strings.TrimSpace(ele.privateKey), ""
		} else if len(strings.TrimSpace(ele.publicKey)) > 0 {
			return "", strings.TrimSpace(ele.publicKey)
		}
	}
	return "", ""
}

func getA2ORCSRootCA(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.Sys != nil && ele.Global.V1.Sys.Tls != nil && len(strings.TrimSpace(ele.Global.V1.Sys.Tls.RootCertContents)) > 0 {
			return ele.Global.V1.Sys.Tls.RootCertContents
		}
	}
	return ""
}

func getA2fqdn(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global != nil && ele.Global.V1 != nil && len(strings.TrimSpace(ele.Global.V1.Fqdn.Value)) > 0 {
			return ele.Global.V1.Fqdn.Value
		}
	}
	return ""
}

func getS3Bucket(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.External.Opensearch != nil && ele.Global.V1.External.Opensearch.Backup != nil && ele.Global.V1.External.Opensearch.Backup.S3 != nil && len(strings.TrimSpace(ele.Global.V1.External.Opensearch.Backup.S3.Bucket.Value)) > 0 {
			return ele.Global.V1.External.Opensearch.Backup.S3.Bucket.Value
		}
	}
	return ""
}

func getOsRoleArn(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.External.Opensearch != nil && ele.Global.V1.External.Opensearch.Backup != nil && ele.Global.V1.External.Opensearch.Backup.S3 != nil && ele.Global.V1.External.Opensearch.Backup.S3.Settings != nil && len(strings.TrimSpace(ele.Global.V1.External.Opensearch.Backup.S3.Settings.RoleArn.Value)) > 0 {
			return ele.Global.V1.External.Opensearch.Backup.S3.Settings.RoleArn.Value
		}
	}
	return ""
}

func getOsAccessKey(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.External.Opensearch != nil && ele.Global.V1.External.Opensearch.Auth != nil && ele.Global.V1.External.Opensearch.Auth.AwsOs != nil && len(strings.TrimSpace(ele.Global.V1.External.Opensearch.Auth.AwsOs.AccessKey.Value)) > 0 {
			return ele.Global.V1.External.Opensearch.Auth.AwsOs.AccessKey.Value
		}
	}
	return ""
}

func getOsSecretKey(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.External.Opensearch != nil && ele.Global.V1.External.Opensearch.Auth != nil && ele.Global.V1.External.Opensearch.Auth.AwsOs != nil && len(strings.TrimSpace(ele.Global.V1.External.Opensearch.Auth.AwsOs.SecretKey.Value)) > 0 {
			return ele.Global.V1.External.Opensearch.Auth.AwsOs.SecretKey.Value
		}
	}
	return ""
}

func getOpenSearchObjectStorageConfig(config map[string]*dc.AutomateConfig) *ObjectStorageConfig {
	objStoage := &ObjectStorageConfig{}
	objStoage.accessKey = getOsAccessKey(config)
	objStoage.secrectKey = getOsSecretKey(config)
	objStoage.RoleArn = getOsRoleArn(config)
	objStoage.bucketName = getS3Bucket(config)
	return objStoage
}

func getJsonFromTerraformTfVarsFile(jsonString string) (*HATfvars, error) {
	params := HATfvars{}
	err := json.Unmarshal([]byte(jsonString), &params)
	if err != nil {
		writer.Fail(err.Error())
		return nil, err
	}
	return &params, nil

}
func getJsonFromTerraformAwsAutoTfVarsFile(jsonString string) (*HAAwsAutoTfvars, error) {
	params := HAAwsAutoTfvars{}
	err := json.Unmarshal([]byte(jsonString), &params)
	if err != nil {
		writer.Fail(err.Error())
		return nil, err
	}
	return &params, nil

}

func getModeOfDeployment() string {
	contentByte, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch")) // nosemgrep
	if err != nil {
		writer.Println(err.Error())
		return ""
	}
	deploymentMode := strings.TrimSpace(string(contentByte))
	if strings.EqualFold(deploymentMode, "existing_nodes") {
		return EXISTING_INFRA_MODE
	} else if strings.EqualFold(deploymentMode, "aws") {
		return AWS_MODE
	}
	return AWS_MODE
}

func getS3BackConfig(config map[string]*dc.AutomateConfig) *ObjectStorageConfig {
	objStoage := &ObjectStorageConfig{}
	for _, ele := range config {
		if ele.Global.V1.Backups != nil && ele.Global.V1.Backups.S3 != nil {
			if ele.Global.V1.Backups.S3.Credentials != nil {
				objStoage.accessKey = ele.Global.V1.Backups.S3.Credentials.AccessKey.Value
				objStoage.secrectKey = ele.Global.V1.Backups.S3.Credentials.SecretKey.Value
			}
			if ele.Global.V1.Backups.S3.Bucket != nil {
				objStoage.bucketName = ele.Global.V1.Backups.S3.Bucket.Name.Value
				objStoage.endpoint = ele.Global.V1.Backups.S3.Bucket.Endpoint.Value
			}
			break
		}
	}
	return objStoage
}
