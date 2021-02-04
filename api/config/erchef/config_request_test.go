package erchef

import (
	"testing"

	"github.com/chef/automate/api/config/shared"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestDataCollector(t *testing.T) {
	t.Run("enabled when External Automate is enabled and Internal Automate is disabled", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.ExternalAutomate = &shared.External_Automate{
			Enable: w.Bool(true),
		}
		c.ConfigureProduct(&shared.ProductConfig{
			Products: []string{"chef-server"},
		})
		require.True(t, c.V1.Sys.GetDataCollector().GetEnabled().GetValue())
	})
	t.Run("enabled when External Automate is disabled and Internal Automate is enabled", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.ConfigureProduct(&shared.ProductConfig{
			Products: []string{"automate", "chef-server"},
		})
		require.True(t, c.V1.Sys.GetDataCollector().GetEnabled().GetValue())
	})
	t.Run("disabled when External Automate is disabled and Internal Automate is disabled", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.ConfigureProduct(&shared.ProductConfig{
			Products: []string{"chef-server"},
		})
		require.False(t, c.V1.Sys.GetDataCollector().GetEnabled().GetValue())
	})
}
