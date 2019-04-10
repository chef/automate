package gateway_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	cr "github.com/chef/automate/api/config/gateway"
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateNewConfigRequest(t *testing.T) {
	t.Run("bare-bones config returns no error", func(t *testing.T) {
		assert.Nil(t, cr.NewConfigRequest().Validate())
	})
}

func TestValidateDefaultConfigRequest(t *testing.T) {
	assert.Nil(t, cr.DefaultConfigRequest().Validate())
}

func TestSetGlobalConfig(t *testing.T) {
	c := cr.DefaultConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("localhost")

	g := ac.NewGlobalConfig()
	g.V1.Fqdn = w.String("test")
	g.V1.Mlsa = &ac.Mlsa{
		Accept: w.Bool(true),
	}
	g.V1.Log = &ac.Log{Level: w.String("debug")}

	c.SetGlobalConfig(g)

	assert.True(t, c.V1.Sys.Mlsa.Accept.Value)
	assert.Equal(t, "test", c.V1.Sys.Service.ExternalFqdn.Value)
	assert.Nil(t, c.Validate())
}

func TestDefaultConfigRequestSupportsOldLogLevelConfig(t *testing.T) {
	c := cr.DefaultConfigRequest()
	assert.Equal(t, "0.0.0.0", c.V1.Sys.Service.Host.Value)
	assert.EqualValues(t, 2000, c.V1.Sys.Service.Port.Value)
	assert.EqualValues(t, 2001, c.V1.Sys.Service.GrpcPort.Value)
	assert.Equal(t, cr.ValidTrialLicenseURLs[0], c.V1.Sys.Service.TrialLicenseUrl.Value)
	assert.Equal(t, "info", c.V1.Sys.Service.Log.Level.Value)
}

func TestNewLogLevelConfigRequest(t *testing.T) {
	c := cr.DefaultConfigRequest()
	// Set the new log level, this will override the old log level config
	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")

	assert.Equal(t, "info", c.V1.Sys.Log.Level.Value)
	assert.Equal(t, "text", c.V1.Sys.Log.Format.Value)
}
