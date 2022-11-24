package main

import (
	"fmt"
	"io/ioutil"
	"os/exec"
	"path/filepath"
	"strings"

	dc "github.com/chef/automate/api/config/deployment"
	mtoml "github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/toml"
)

var tfVarRbTemp = `
require 'json'

%s

puts({
    :region => region,
    :endpoint => endpoint,
    :secret_key => secret_key,
    :access_key => access_key,
    :bucket_name => bucket_name,
    :ssh_key_file => ssh_key_file,
    :ssh_port => ssh_port,
    :ssh_user => ssh_user,
    :habitat_uid_gid => habitat_uid_gid,
    :postgresql_archive_disk_fs_path => postgresql_archive_disk_fs_path,
    :postgresql_instance_count => postgresql_instance_count,
    :nfs_mount_path => nfs_mount_path,
    :opensearch_certs_by_ip => opensearch_certs_by_ip,
    :postgresql_certs_by_ip => postgresql_certs_by_ip,
    :chef_server_certs_by_ip => chef_server_certs_by_ip,
    :automate_certs_by_ip => automate_certs_by_ip,
    :opensearch_nodes_dn => opensearch_nodes_dn,
    :opensearch_admin_dn => opensearch_admin_dn,
    :opensearch_custom_certs_enabled => opensearch_custom_certs_enabled,
    :postgresql_custom_certs_enabled => postgresql_custom_certs_enabled,
    :chef_server_custom_certs_enabled => chef_server_custom_certs_enabled,
    :automate_custom_certs_enabled => automate_custom_certs_enabled,
    :postgresql_public_key => postgresql_public_key,
    :opensearch_admin_cert => opensearch_admin_cert,
    :opensearch_public_key => opensearch_public_key,
    :chef_server_public_key => chef_server_public_key,
    :automate_public_key => automate_public_key,
    :postgresql_private_key => postgresql_private_key,
    :opensearch_private_key => opensearch_private_key,
    :opensearch_admin_key => opensearch_admin_key,
    :chef_server_private_key => chef_server_private_key,
    :automate_private_key => automate_private_key,
    :postgresql_root_ca => postgresql_root_ca,
    :opensearch_root_ca => opensearch_root_ca,
    :automate_root_ca => automate_root_ca,
    :opensearch_instance_count => opensearch_instance_count,
    :chef_server_instance_count => chef_server_instance_count,
    :automate_instance_count => automate_instance_count,
    :automate_fqdn => automate_fqdn,
    :automate_config_file => automate_config_file,
    :opensearch_root_cert => opensearch_root_cert,
    :postgresql_root_cert => postgresql_root_cert,
    :managed_rds_postgresql_certificate => managed_rds_postgresql_certificate,
    :managed_rds_dbuser_password => managed_rds_dbuser_password,
    :managed_rds_dbuser_username => managed_rds_dbuser_username,
    :managed_rds_superuser_password => managed_rds_superuser_password,
    :managed_rds_superuser_username => managed_rds_superuser_username,
    :managed_rds_instance_url => managed_rds_instance_url,
    :os_snapshot_user_access_key_secret => os_snapshot_user_access_key_secret,
    :os_snapshot_user_access_key_id => os_snapshot_user_access_key_id,
    :aws_os_snapshot_role_arn => aws_os_snapshot_role_arn,
    :managed_opensearch_certificate => managed_opensearch_certificate,
    :managed_opensearch_user_password => managed_opensearch_user_password,
    :managed_opensearch_username => managed_opensearch_username,
    :managed_opensearch_domain_url => managed_opensearch_domain_url,
    :managed_opensearch_domain_name => managed_opensearch_domain_name,
    :setup_self_managed_services => setup_self_managed_services,
    :setup_managed_services => setup_managed_services,
    :existing_postgresql_private_ips => existing_postgresql_private_ips,
    :existing_opensearch_private_ips => existing_opensearch_private_ips,
    :existing_chef_server_private_ips => existing_chef_server_private_ips,
    :existing_automate_private_ips => existing_automate_private_ips,
    :backup_config_s3 => backup_config_s3,
}.to_json)
`

type ConfigKeys struct {
	rootCA     string
	privateKey string
	publicKey  string
	adminCert  string
	adminKey   string
}

type PullConfigs interface {
	pullOpensearchConfigs() (map[string]*ConfigKeys, error)
	pullPGConfigs() (map[string]*ConfigKeys, error)
	pullAutomateConfigs() (map[string]*dc.AutomateConfig, error)
	pullChefServerConfigs() (map[string]*dc.AutomateConfig, error)
	generateConfig() error
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

func (p *PullConfigsImpl) generateConfig() error {
	osConfigMap, err := p.pullOpensearchConfigs()
	if err != nil {
		return err
	}
	pgConfigMap, err := p.pullPGConfigs()
	if err != nil {
		return err
	}
	a2ConfigMap, err := p.pullAutomateConfigs()
	if err != nil {
		return err
	}
	csConfigMap, err := p.pullChefServerConfigs()
	if err != nil {
		return err
	}

	var sharedConfigToml *ExistingInfraConfigToml
	if checkSharedConfigFile() {
		sharedConfigToml, err = getExistingInfraConfig(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml"))
		if err != nil {
			return err
		}
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

	var a2Certs []CertByIP
	for key, ele := range a2ConfigMap {
		fmt.Printf("Automate Config........%s\n", key)
		fmt.Println(ele.Global.V1.FrontendTls)
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
		fmt.Printf("Chef-Server Config........ %s\n", key)
		fmt.Println(ele.Global.V1.FrontendTls)
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
		return err
	}
	err = ioutil.WriteFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config_1.toml"), shardConfig, 600)
	if err != nil {
		return err
	}
	return nil
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
		if len(strings.TrimSpace(ele.rootCA)) > 0 {
			return strings.TrimSpace(ele.adminCert), strings.TrimSpace(ele.adminKey)
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

func readTerraformTfVarsFile() {
	contentByte, err := ioutil.ReadFile(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "terraform.tfvars"))
	if err != nil {
		writer.Println(err.Error())
	}
	rbScript := fmt.Sprintf(tfVarRbTemp, string(contentByte))
	filenamePath := filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "tfvar_json_script.rb")
	err = ioutil.WriteFile(filenamePath, []byte(rbScript), 777)
	if err != nil {
		writer.Println(err.Error())
	}

	pkgcmd := "HAB_LICENSE=accept-no-persist hab pkg path core/ruby30"
	out, err := exec.Command("/bin/sh", "-c", pkgcmd).Output()
	if err != nil {
		writer.Fail(err.Error())
	}
	rubuyBin := string(out)
	rubuyBin = rubuyBin + "/hab/pkgs/core/ruby30/3.0.3/20220312100602/bin/ruby"
	err = executeCommand("/hab/pkgs/core/ruby30/3.0.3/20220312100602/bin/ruby", []string{filenamePath}, "")
	if err != nil {
		writer.Println(err.Error())
	}

}
