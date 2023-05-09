package systemresourcechecktrigger

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/require"
)

func TestSystemResourceCheck_Run(t *testing.T) {
	t.Run("Test 1. empty val", func(t *testing.T) {
		ss := NewSystemResourceCheck("127.0.0.1", "80")
		config := models.Config{}
		result := ss.Run(config)
		require.Empty(t, result)
	})

	t.Run("Test 2. models.CheckTriggerResponse map", func(t *testing.T) {
		ss := NewSystemResourceCheck("127.0.0.1", "80")
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:   []string{"127.0.0.1"},
				AutomateNodeCount: 1,
			},
		}
		result := ss.Run(config)
		require.NotEmpty(t, result)
		require.NotNil(t, result["127.0.0.1"])
	})

	t.Run("Test 3. not-reachable-api", func(t *testing.T) {
		ss := NewSystemResourceCheck("127.0.0.1", "80")
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:   []string{"not-reachable-api"},
				AutomateNodeCount: 1,
			},
		}
		result := ss.Run(config)
		require.NotEmpty(t, result)
		require.NotNil(t, result["not-reachable-api"])
	})
}
