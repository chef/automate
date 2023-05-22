package config_verify

import (
	sc "github.com/chef/automate/api/config/deployment"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func GetAutomateScConfigTestData() *sc.AutomateConfig {

	c := sc.NewAutomateConfig()
	c.Global.V1.Fqdn = w.String("test.com")
	c.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
	c.Deployment.V1.Svc.DeploymentType = w.String("local")
	c.Deployment.V1.Svc.Channel = w.String("current")
	c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
	c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")

	return c
}
