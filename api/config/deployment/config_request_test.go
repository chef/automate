package deployment

import (
	"testing"

	"github.com/stretchr/testify/assert"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestMissingDeploymentType(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.DeploymentType = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.deployment_type")

	assert.EqualError(t, c.Validate(), expected.Error(), "")
}

func TestValidateConfigRequestInvalidDeploymentType(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.DeploymentType = w.String("ssh")

	expected := config.NewInvalidConfigError()
	expected.AddInvalidValue("deployment.v1.svc.deployment_type", "The only supported deployment type is 'local'")

	assert.EqualError(t, c.Validate(), expected.Error(), "")
}

func TestValidateConfigRequestMissingChannel(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.Channel = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.channel")

	assert.EqualError(t, c.Validate(), expected.Error(), "")
}

func TestValidateConfigRequestInvalidChannel(t *testing.T) {
	tests := map[string]bool{
		"dev":        true,
		"acceptance": true,
		"current":    true,
		"unstable":   false,
		"cowboy":     false,
	}

	for channel, pass := range tests {
		c := newValidTestConfigRequest()
		c.V1.Svc.Channel = w.String(channel)
		err := c.Validate()

		if pass {
			assert.Nil(t, err)
		} else {
			expected := config.NewInvalidConfigError()
			expected.AddInvalidValue("deployment.v1.svc.channel", "Channel must be one of: current, acceptance, dev")
			assert.EqualError(t, err, expected.Error(), "")
		}
	}
}

func TestValidateConfigRequestInvalidUpgradeStrategy(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.UpgradeStrategy = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.upgrade_strategy")

	assert.EqualError(t, c.Validate(), expected.Error(), "")
}

func TestValidateConfigRequestMissingUpgradeStrategy(t *testing.T) {
	tests := map[string]bool{
		"none":    true,
		"at-once": true,
		"cowboy":  false,
	}

	for strategy, pass := range tests {
		c := newValidTestConfigRequest()
		c.V1.Svc.UpgradeStrategy = w.String(strategy)

		if pass {
			assert.Nil(t, c.Validate())
		} else {
			expected := config.NewInvalidConfigError()
			expected.AddInvalidValue("deployment.v1.svc.upgrade_strategy", "Upgrade strategy must be one of: none, at-once")
			assert.EqualError(t, c.Validate(), expected.Error(), "")
		}
	}
}

func TestValidateConfigRequestMissingAdminUsername(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.AdminUser.Username = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.admin_user.username")

	assert.EqualError(t, c.ValidateCredentials(), expected.Error(), "")
}

func TestValidateConfigRequestMissingAdminName(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.AdminUser.Name = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.admin_user.name")

	assert.EqualError(t, c.ValidateCredentials(), expected.Error(), "")
}

func TestValidateConfigRequestMissingAdminPassword(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Svc.AdminUser.Password = nil

	expected := config.NewInvalidConfigError()
	expected.AddMissingKey("deployment.v1.svc.admin_user.password")

	assert.EqualError(t, c.ValidateCredentials(), expected.Error(), "")
}

func TestValidateConfigRequestValid(t *testing.T) {
	c := newValidTestConfigRequest()

	assert.Nil(t, c.Validate())
}

func newValidTestConfigRequest() *ConfigRequest {
	// We should use DefaultConfigRequest after we've populated it with default
	// config.
	c := NewConfigRequest()
	c.V1.Svc.DeploymentType = w.String("local")
	c.V1.Svc.Channel = w.String("current")
	c.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.V1.Svc.AdminUser.Name = w.String("cow boy")
	c.V1.Svc.AdminUser.Username = w.String("cowboy")
	c.V1.Svc.AdminUser.Password = w.String("ponies")
	c.V1.Svc.ManifestCacheExpiry = w.String("5m")

	return c
}

func TestValidateConfigRequestProxyConnectionString(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Sys.Proxy.ConnectionString = w.String("https://172.28.5.249:3128")

	// TODO: change this to assert.NoError when Validate() returns an error interface
	assert.Nil(t, c.Validate(), "expected config to be valid with a proxy connection string set")
}

func TestValidateConfigRequestNoProxyString(t *testing.T) {
	c := newValidTestConfigRequest()
	c.V1.Sys.Proxy.NoProxyString = w.String("packages.chef.io,raw.githubusercontent.com,api.bintray.com,bldr.habitat.sh,akamai.bintray.com,dl.bintray.com,bintray.com")

	// TODO: change this to assert.NoError when Validate() returns an error interface
	assert.Nil(t, c.Validate(), "expected config to be valid with a no proxy string set")
}
