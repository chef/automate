package authz

import (
	"testing"

	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/stretchr/testify/assert"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := DefaultConfigRequest()
	err := c.Validate()
	assert.NoError(t, err)
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA",
		func(t *testing.T) {
			c := NewConfigRequest()
			c.V1.Sys.Mlsa.Accept = w.Bool(false)
			g := shared.DefaultGlobalConfig()
			g.V1.Mlsa.Accept = w.Bool(true)

			c.SetGlobalConfig(g)

			assert.True(t, c.V1.Sys.Mlsa.Accept.Value)
		},
	)
}

func TestMaxConnectors(t *testing.T) {
	t.Run("it configures the max connectors for postgres",
		func(t *testing.T) {
			c := NewConfigRequest()
			c.V1.Sys.Storage.MaxConnections = w.Int32(5)

			assert.Equal(t, int32(5), c.V1.Sys.Storage.MaxConnections.Value)
		},
	)
}
