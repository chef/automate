package local_user

import (
	"testing"

	"github.com/stretchr/testify/assert"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA",
		func(t *testing.T) {
			c := newValidConfigRequest()
			c.V1.Sys.Mlsa.Accept = w.Bool(false)
			g := config.DefaultGlobalConfig()
			g.V1.Mlsa.Accept = w.Bool(true)

			c.SetGlobalConfig(g)

			assert.Equal(t, c.V1.Sys.Mlsa.Accept.Value, true)
		},
	)
	t.Run("it sets the FQDN",
		func(t *testing.T) {
			c := newValidConfigRequest()
			c.V1.Sys.Service.ExternalFqdn = w.String("localhost")

			g := config.DefaultGlobalConfig()
			g.V1.Fqdn = w.String("external_fqdn")

			c.SetGlobalConfig(g)

			assert.Equal(t, "external_fqdn", c.V1.Sys.Service.ExternalFqdn.Value)
		},
	)
}

func newValidConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("localhost")

	return c
}
