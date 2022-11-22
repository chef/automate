package main

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"

	dc "github.com/chef/automate/api/config/deployment"
	mtoml "github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/toml"
)

type ConfigKeys struct {
	rootCA     string
	privateKey string
	publicKey  string
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

	var a2Certs []CertByIP
	for key, ele := range a2ConfigMap {
		certByIP := CertByIP{
			IP:         key,
			PrivateKey: ele.Global.V1.FrontendTls[0].Key,
			PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
		}
		pgCerts = append(a2Certs, certByIP)
	}

	sharedConfigToml.Automate.Config.CertsByIP = a2Certs

	var csCerts []CertByIP
	for key, ele := range csConfigMap {
		certByIP := CertByIP{
			IP:         key,
			PrivateKey: ele.Global.V1.FrontendTls[0].Key,
			PublicKey:  ele.Global.V1.FrontendTls[0].Cert,
		}
		pgCerts = append(csCerts, certByIP)
	}

	sharedConfigToml.ChefServer.Config.CertsByIP = csCerts

	shardConfig, err := mtoml.Marshal(sharedConfigToml)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile("config_1.toml", shardConfig, 600)
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

func getA2ORCSRootCA(config map[string]*dc.AutomateConfig) string {
	for _, ele := range config {
		if len(strings.TrimSpace(ele.Global.V1.Sys.Tls.RootCertContents)) > 0 {
			return ele.Global.V1.Sys.Tls.RootCertContents
		}
	}
	return ""
}

func getCSRootCA(config map[string]*dc.AutomateConfig) {

}
