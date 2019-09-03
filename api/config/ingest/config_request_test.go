package ingest

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
	c.V1.Sys.Service.PurgeConvergeHistoryAfterDays = w.Int32(7)
	c.V1.Sys.Service.PurgeActionsAfterDays = w.Int32(7)
	err := c.Validate()
	require.Error(t, err)
	require.Contains(t, err.Error(), "'ingest.v1.sys.service.purge_converge_history_after_days' has been deprecated")
	require.Contains(t, err.Error(), "'ingest.v1.sys.service.purge_actions_after_days' has been deprecated")
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
