package config_verify

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func GetFAiledAutomateScConfigTestData() *sc.AutomateConfig {

	c := sc.NewAutomateConfig()
	c.Global.V1.Fqdn = w.String(".com")
	c.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
	c.Deployment.V1.Svc.DeploymentType = w.String("test")
	c.Deployment.V1.Svc.Channel = w.String("none")
	c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
	c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")

	return c
}

func validTLSCredentialForTest() *shared.FrontendTLSCredential {
	return &shared.FrontendTLSCredential{
		ServerName: "test.example",
		CertPath:   "./testdata/admin.crt",
		KeyPath:    "./testdata/admin.pem",
	}
}

func validTLSCredentialSliceForTest() []*shared.FrontendTLSCredential {
	return []*shared.FrontendTLSCredential{validTLSCredentialForTest()}
}
