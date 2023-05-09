package systemuserchecktrigger

import (
	"fmt"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/require"
)

func TestSystemUserCheck_Run(t *testing.T) {
	t.Run("Test 1. empty value", func(t *testing.T) {
		ss := NewSystemUserCheck("127.0.0.1", "80")
		config := models.Config{}
		result := ss.Run(config)
		require.Empty(t, result)
	})

	t.Run("Test 2. get models.CheckTriggerResponse map", func(t *testing.T) {
		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(trigger.softwareVersionResp))
		}))
		defer mockServer.Close()
		ss := NewSystemUserCheck(mockServer.URL, "80")
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:   []string{mockServer.URL},
				AutomateNodeCount: 1,
			},
		}
		result := ss.Run(config)

		require.NotEmpty(t, result)
		fmt.Printf("mockServer.URL: %v\n", mockServer.URL)
		require.NotNil(t, result[mockServer.URL])

	})

	t.Run("Test 3. not reachable api", func(t *testing.T) {
		ss := NewSystemUserCheck("127.0.0.1", "80")
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:   []string{"not-reachable-api"},
				AutomateNodeCount: 1,
			},
		}
		result := ss.Run(config)
		fmt.Printf("result: %v\n", result)
		require.NotEmpty(t, result)
	})
}
