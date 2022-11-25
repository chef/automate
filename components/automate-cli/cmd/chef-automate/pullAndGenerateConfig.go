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
	mtoml "github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/toml"
	"github.com/sirupsen/logrus"
)

var tfVarRbTemp = `
require 'json'

%s

puts({
    %s
}.to_json)
`

type ConfigKeys struct {
	rootCA     string
	privateKey string
	publicKey  string
	adminCert  string
	adminKey   string
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
}
type PullConfigs interface {
	pullOpensearchConfigs() (map[string]*ConfigKeys, error)
	pullPGConfigs() (map[string]*ConfigKeys, error)
	pullAutomateConfigs() (map[string]*dc.AutomateConfig, error)
	pullChefServerConfigs() (map[string]*dc.AutomateConfig, error)
	generateConfig() (*ExistingInfraConfigToml, error)
}

type PullConfigsImpl struct {
	infra   *AutomteHAInfraDetails
	sshUtil SSHUtil
}

func NewPullConfigs(infra *AutomteHAInfraDetails, sshUtil SSHUtil) PullConfigs {
	return &PullConfigsImpl{
		infra:   infra,
		sshUtil: sshUtil,
	}
}

func (p *PullConfigsImpl) pullOpensearchConfigs() (map[string]*ConfigKeys, error) {
	ipConfigMap := make(map[string]*ConfigKeys)
	for _, ip := range p.infra.Outputs.OpensearchPrivateIps.Value {
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
		p.sshUtil.getSSHConfig().hostIP = ip
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(GET_FRONTEND_CONFIG, true)
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
		p.sshUtil.getSSHConfig().hostIP = ip
		rawOutput, err := p.sshUtil.connectAndExecuteCommandOnRemote(GET_FRONTEND_CONFIG, true)
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

func (p *PullConfigsImpl) generateConfig() (*ExistingInfraConfigToml, error) {
	sharedConfigToml, err := getHAConfig()
	if err != nil {
		return nil, err
	}
	a2ConfigMap, err := p.pullAutomateConfigs()
	if err != nil {
		return nil, err
	}
	csConfigMap, err := p.pullChefServerConfigs()
	if err != nil {
		return nil, err
	}
	// checking onprem with managed or self managed services
	logrus.Debug(sharedConfigToml.ExternalDB.Database.Type)
	if len(strings.TrimSpace(sharedConfigToml.ExternalDB.Database.Type)) < 1 {
		osConfigMap, err := p.pullOpensearchConfigs()
		if err != nil {
			return nil, err
		}
		pgConfigMap, err := p.pullPGConfigs()
		if err != nil {
			return nil, err
		}

		var osCerts []CertByIP
		for key, ele := range osConfigMap {
			certByIP := CertByIP{
				IP:         key,
				PrivateKey: ele.privateKey,
				PublicKey:  ele.publicKey,
			}
			osCerts = append(osCerts, certByIP)
		}

		sharedConfigToml.Opensearch.Config.CertsByIP = osCerts

		sharedConfigToml.Opensearch.Config.RootCA = getOSORPGRootCA(osConfigMap)

		sharedConfigToml.Opensearch.Config.AdminCert, sharedConfigToml.Opensearch.Config.AdminKey = getOSAdminCertAndAdminKey(osConfigMap)

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

		sharedConfigToml.Postgresql.Config.RootCA = getOSORPGRootCA(pgConfigMap)
	}

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

	sharedConfigToml.Automate.Config.RootCA = getA2ORCSRootCA(a2ConfigMap)

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

	shardConfig, err := mtoml.Marshal(sharedConfigToml)
	if err != nil {
		return nil, err
	}
	err = ioutil.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"), shardConfig, 0644)
	if err != nil {
		return nil, err
	}
	return sharedConfigToml, nil
}

func getHAConfig() (*ExistingInfraConfigToml, error) {
	if checkSharedConfigFile() {
		sharedConfigToml, err := getExistingInfraConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return nil, err
		}
		return sharedConfigToml, nil
	} else {
		contentByte, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "terraform.tfvars"))
		if err != nil {
			writer.Println(err.Error())
			return nil, err
		}
		tfvarConfig, err := getJsonFromTerraformTfVarsFile(string(contentByte))
		if err != nil {
			return nil, err
		}
		return getHAConfigFromTFVars(tfvarConfig)
	}
}

func getHAConfigFromTFVars(tfvarConfig *HATfvars) (*ExistingInfraConfigToml, error) {
	sharedConfigToml := &ExistingInfraConfigToml{}
	sharedConfigToml.Architecture.ConfigInitials.Architecture = "existing_nodes"
	sharedConfigToml.Architecture.ConfigInitials.SecretsKeyFile = tfvarConfig.SecretKey
	if strings.EqualFold(tfvarConfig.BackupConfigS3, "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "object_storage"
	}
	if strings.EqualFold(tfvarConfig.BackupConfigEFS, "true") {
		sharedConfigToml.Architecture.ConfigInitials.BackupConfig = "file_system"
	}
	sharedConfigToml.Architecture.ConfigInitials.BackupMount = tfvarConfig.NfsMountPath
	sharedConfigToml.Architecture.ConfigInitials.HabitatUIDGid = tfvarConfig.HabitatUidGid
	sharedConfigToml.Architecture.ConfigInitials.SSHKeyFile = tfvarConfig.SshKeyFile
	sharedConfigToml.Architecture.ConfigInitials.SSHPort = tfvarConfig.SshPort
	sharedConfigToml.Architecture.ConfigInitials.SSHUser = tfvarConfig.SshUser
	sharedConfigToml.Automate.Config.Fqdn = tfvarConfig.AutomateFqdn
	sharedConfigToml.Automate.Config.InstanceCount = strconv.Itoa(tfvarConfig.AutomateInstanceCount)
	sharedConfigToml.Automate.Config.ConfigFile = tfvarConfig.AutomateConfigFile
	sharedConfigToml.Automate.Config.EnableCustomCerts = tfvarConfig.AutomateCustomCertsEnabled
	sharedConfigToml.Automate.Config.AdminPassword = tfvarConfig.AutomateAdminPassword
	sharedConfigToml.Automate.Config.TeamsPort = strconv.Itoa(tfvarConfig.TeamsPort)
	sharedConfigToml.ChefServer.Config.EnableCustomCerts = tfvarConfig.ChefServerCustomCertsEnabled
	sharedConfigToml.ChefServer.Config.InstanceCount = strconv.Itoa(tfvarConfig.ChefServerInstanceCount)
	sharedConfigToml.Postgresql.Config.EnableCustomCerts = tfvarConfig.PostgresqlCustomCertsEnabled
	sharedConfigToml.Postgresql.Config.InstanceCount = strconv.Itoa(tfvarConfig.PostgresqlInstanceCount)
	sharedConfigToml.Opensearch.Config.EnableCustomCerts = tfvarConfig.OpensearchCustomCertsEnabled
	sharedConfigToml.Opensearch.Config.InstanceCount = strconv.Itoa(tfvarConfig.OpensearchInstanceCount)
	sharedConfigToml.Opensearch.Config.AdminDn = tfvarConfig.OpensearchAdminDn
	sharedConfigToml.Opensearch.Config.NodesDn = tfvarConfig.OpensearchNodesDn
	sharedConfigToml.ExistingInfra.Config.AutomatePrivateIps = tfvarConfig.ExistingAutomatePrivateIps
	sharedConfigToml.ExistingInfra.Config.ChefServerPrivateIps = tfvarConfig.ExistingChefServerPrivateIps
	sharedConfigToml.ExistingInfra.Config.PostgresqlPrivateIps = tfvarConfig.ExistingPostgresqlPrivateIps
	sharedConfigToml.ExistingInfra.Config.OpensearchPrivateIps = tfvarConfig.ExistingOpensearchPrivateIps
	sharedConfigToml.ObjectStorage.Config.AccessKey = tfvarConfig.AccessKey
	sharedConfigToml.ObjectStorage.Config.SecretKey = tfvarConfig.SecretKey
	sharedConfigToml.ObjectStorage.Config.BucketName = tfvarConfig.BucketName
	sharedConfigToml.ObjectStorage.Config.Endpoint = tfvarConfig.Endpoint
	sharedConfigToml.ObjectStorage.Config.Region = tfvarConfig.Region
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchInstanceURL = tfvarConfig.ManagedOpensearchDomainUrl
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchDomainName = tfvarConfig.ManagedOpensearchDomainName
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchCertificate = tfvarConfig.ManagedOpensearchCertificate
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchRootCert = tfvarConfig.OpensearchRootCert
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchSuperUserName = tfvarConfig.ManagedOpensearchUsername
	sharedConfigToml.ExternalDB.Database.Opensearch.OpensearchSuperUserPassword = tfvarConfig.ManagedOpensearchUserPassword
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.AwsOsSnapshotRoleArn = tfvarConfig.AwsOsSnapshotRoleArn
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeyId = tfvarConfig.OsSnapshotUserAccessKeyId
	sharedConfigToml.ExternalDB.Database.Opensearch.AWS.OsUserAccessKeySecret = tfvarConfig.OsSnapshotUserAccessKeySecret
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLCertificate = tfvarConfig.ManagedRdsPostgresqlCertificate
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserName = tfvarConfig.ManagedRdsDbuserUsername
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLDBUserPassword = tfvarConfig.ManagedRdsDbuserPassword
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLInstanceURL = tfvarConfig.ManagedRdsInstanceUrl
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLRootCert = tfvarConfig.PostgresqlRootCert
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserName = tfvarConfig.ManagedRdsSuperuserUsername
	sharedConfigToml.ExternalDB.Database.PostgreSQL.PostgreSQLSuperUserPassword = tfvarConfig.ManagedRdsSuperuserPassword
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

func getOSORPGRootCA(config map[string]*ConfigKeys) string {
	for _, ele := range config {
		if len(strings.TrimSpace(ele.rootCA)) > 0 {
			return strings.TrimSpace(ele.rootCA)
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

func getA2ORCSRootCA(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if ele.Global.V1.Sys != nil && ele.Global.V1.Sys.Tls != nil && len(strings.TrimSpace(ele.Global.V1.Sys.Tls.RootCertContents)) > 0 {
			return ele.Global.V1.Sys.Tls.RootCertContents
		}
	}
	return ""
}

func getJsonFromTerraformTfVarsFile(tfvarsContent string) (*HATfvars, error) {
	rbScript := fmt.Sprintf(tfVarRbTemp, tfvarsContent, getKeysFromTfvars(tfvarsContent))
	filenamePath := filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "tfvar_json_script.rb")
	err := ioutil.WriteFile(filenamePath, []byte(rbScript), 777)
	if err != nil {
		writer.Println(err.Error())
	}

	pkgcmd := "HAB_LICENSE=accept-no-persist hab pkg path core/ruby30"
	out, err := exec.Command("/bin/sh", "-c", pkgcmd).Output()
	if err != nil {
		writer.Fail(err.Error())
		return nil, err
	}
	rubyBin := string(out)
	rubyBin = strings.TrimSpace(rubyBin) + "/bin/ruby"

	jsonStr, err := exec.Command(rubyBin, filenamePath).Output()
	if err != nil {
		writer.Println(err.Error())
		return nil, err
	}
	params := HATfvars{}
	err = json.Unmarshal(jsonStr, &params)
	if err != nil {
		writer.Fail(err.Error())
		return nil, err
	}
	return &params, nil

}

func getKeysFromTfvars(text string) string {
	var keys string
	tokens := strings.Split(text, "\n")
	for _, token := range tokens {
		if len(token) > 0 && strings.Contains(token, "=") {
			key := strings.TrimSpace(token[0:strings.Index(token, "=")])
			keys = keys + ":" + key + " => " + key + ", \n"
		}
	}
	return keys
}

func getModeOfDeployment() string {
	contentByte, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", ".tf_arch"))
	if err != nil {
		writer.Println(err.Error())
		return ""
	}
	deploymentMode := strings.TrimSpace(string(contentByte))

	if strings.EqualFold(deploymentMode, "existing_nodes") {
		return EXISTING_INFRA_MODE
	}
	return AWS_MODE
}
