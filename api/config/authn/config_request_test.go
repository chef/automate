package authn_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/config/authn"
	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := authn.NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA",
		func(t *testing.T) {
			c := authn.NewConfigRequest()
			c.V1.Sys.Mlsa.Accept = w.Bool(false)
			g := shared.DefaultGlobalConfig()
			g.V1.Mlsa.Accept = w.Bool(true)

			c.SetGlobalConfig(g)

			assert.True(t, c.V1.Sys.Mlsa.Accept.Value)
		},
	)
}

func TestDefaultConfigRequest(t *testing.T) {
	c := authn.DefaultConfigRequest()
	assert.Equal(t, "0.0.0.0", c.V1.Sys.Service.Host.Value)
	assert.EqualValues(t, 10113, c.V1.Sys.Service.Port.Value)
	assert.Equal(t, "info", c.V1.Sys.Logger.Level.Value)
}
