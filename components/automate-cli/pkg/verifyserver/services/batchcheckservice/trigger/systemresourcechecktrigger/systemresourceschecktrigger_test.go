package systemresourcechecktrigger

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/require"
)

const (
	resourceCheck = `{
		"status": "SUCCESS",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "CPU count check",
					"passed": true,
					"success_msg": "CPU count is >= 4",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "CPU speed check",
					"passed": true,
					"success_msg": "CPU speed should be >= 2Ghz",
					"error_msg": "",
					"resolution_msg": ""
				}
			]
		}
	}`
)

func TestSystemResourceCheck_Run(t *testing.T) {
	t.Run("System Resource Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusOK)
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
			DeploymentState: "pre-release",
		}

		suc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		// Assert the expected result
		require.Len(t, ctr, 1)
		require.Nil(t, ctr[0].Error)
		require.Len(t, ctr[0].Result.Checks, 2)
		require.Equal(t, "SUCCESS", ctr[0].Status)
		checkResp := ctr[0].Result.Checks[1]

		assert.Equal(t, "CPU speed check", checkResp.Title)
		assert.Equal(t, true, checkResp.Passed)
		assert.Equal(t, "CPU speed should be >= 2Ghz", checkResp.SuccessMsg)
		assert.Equal(t, "", checkResp.ErrorMsg)
		assert.Equal(t, "", checkResp.ResolutionMsg)

	})

	t.Run("Failed Resource Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusInternalServerError)
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}

		suc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 1)
		require.NotNil(t, ctr[0].Error)
		require.Equal(t, ctr[0].Error.Code, http.StatusInternalServerError)
		require.Equal(t, "error while connecting to the endpoint, received invalid status code", ctr[0].Error.Error())
	})
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			assert.Equal(t, constants.SYSTEM_RESOURCE_CHECK_API_PATH, r.URL.Path)
			reqParameters := r.URL.Query()
			assert.Equal(t, 2, len(reqParameters))
			assert.Equal(t, "automate", reqParameters.Get("node_type"))
			assert.Equal(t, "pre-release", reqParameters.Get("deployment_state"))

			if r.URL.Path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(resourceCheck))
			}
		}))

		// Extract IP and port from the server's URL
		address := server.URL[strings.Index(server.URL, "//")+2:]
		colonIndex := strings.Index(address, ":")
		ip := address[:colonIndex]
		port := address[colonIndex+1:]

		return server, ip, port
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(requiredStatusCode)
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}
