package ingest

import (
	"testing"

	"github.com/stretchr/testify/assert"

	shared "github.com/chef/automate/api/config/shared"
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
			c := NewConfigRequest()
			c.V1.Sys.Mlsa.Accept = w.Bool(false)
			g := shared.DefaultGlobalConfig()
			g.V1.Mlsa.Accept = w.Bool(true)

			c.SetGlobalConfig(g)

			assert.Equal(t, c.V1.Sys.Mlsa.Accept.Value, true)
		},
	)
}
