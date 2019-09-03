package compliance

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestDeprecations(t *testing.T) {
	c := NewConfigRequest()
	c.V1.Sys.Retention = &ConfigRequest_V1_System_Retention{
		ComplianceReportDays: w.Int32(7),
	}
	err := c.Validate()
	require.Error(t, err)
	require.Contains(t, err.Error(), "'compliance.v1.sys.retention' has been deprecated")
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA", func(t *testing.T) {
		c := NewConfigRequest()
		c.V1.Sys.Mlsa.Accept = w.Bool(false)
		g := shared.DefaultGlobalConfig()
		g.V1.Mlsa.Accept = w.Bool(true)

		c.SetGlobalConfig(g)

		assert.Equal(t, c.V1.Sys.Mlsa.Accept.Value, true)
	})
}
