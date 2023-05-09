package softwareversionchecktrigger

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/require"
)

const (
	softwareVersionResp = `{
		"status": "success",
		"result": {
			"passed": true,
			"msg": "API result message",
			"check": "API check",
			"checks": [
				{
					"title": "Check 1",
					"passed": true,
					"success_msg": "Check 1 passed",
					"error_msg": "",
					"resolution_msg": "No resolution required"
				},
				{
					"title": "Check 2",
					"passed": false,
					"success_msg": "",
					"error_msg": "Check 2 failed",
					"resolution_msg": "Please check the configuration"
				}
			]
		},
		"host": ""
	}`
)

func TestSoftwareVersionCheck_Run(t *testing.T) {
	t.Run("Software Version Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(http.StatusOK)
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}

		suc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		// Assert the expected result
		require.Len(t, ctr, 1)
		require.Nil(t, ctr[host].Error)
		require.Len(t, ctr[host].Result.Checks, 2)

	})

	t.Run("Failed Software Version Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(http.StatusInternalServerError)
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}

		suc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 1)
		require.NotNil(t, ctr[host].Error)
	})
}

// Helper function to create a dummy server
func createDummyServer(requiredStatusCode int) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

			if r.URL.Path == constants.SOFTWARE_VERSION_CHECK_API_PATH {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(softwareVersionResp))
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
