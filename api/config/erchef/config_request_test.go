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

func TestDefaultValues(t *testing.T) {
	defaultValues := DefaultConfigRequest()
	assert.Equal(t, int32(70), defaultValues.GetV1().GetSys().GetDataCollector().GetPoolMaxAge().GetValue())
	assert.Equal(t, int32(1), defaultValues.GetV1().GetSys().GetDataCollector().GetPoolCullInterval().GetValue())
	assert.Equal(t, int32(70), defaultValues.GetV1().GetSys().GetDataCollector().GetMaxConnectionDuration().GetValue())
	assert.Equal(t, int32(10000), defaultValues.GetV1().GetSys().GetDataCollector().GetIbrowseTimeout().GetValue())

	require.Equal(t, int32(0), defaultValues.GetV1().GetSys().GetAuthz().GetCleanupBatchSize().GetValue())

	require.Equal(t, int32(900), defaultValues.GetV1().GetSys().GetApi().GetS3UrlTtl().GetValue())

	require.Equal(t, int32(1), defaultValues.GetV1().GetSys().GetIbrowse().GetIbrowseMaxPipelineSize().GetValue())
	require.Equal(t, int32(256), defaultValues.GetV1().GetSys().GetIbrowse().GetIbrowseMaxSessions().GetValue())

}
