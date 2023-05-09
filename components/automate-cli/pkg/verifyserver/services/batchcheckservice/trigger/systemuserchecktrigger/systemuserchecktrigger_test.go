package systemuserchecktrigger

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
	systemUser = `{
		"status": "SUCCESS",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "User creation/validation check",
					"passed": true,
					"success_msg": "User is created or found successfully",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "User and group mapping successfully",
					"passed": true,
					"success_msg": "User and group mapping successful",
					"error_msg": "",
					"resolution_msg": ""
				}
			]
		}
	}`
)

func TestSystemUserCheck_Run(t *testing.T) {
	t.Run("System User Check", func(t *testing.T) {
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

		suc := NewSystemUserCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		// Assert the expected result
		require.Len(t, ctr, 1)
		require.Nil(t, ctr[host].Error)
		require.Len(t, ctr[host].Result.Checks, 2)

	})

	t.Run("Failed User Check", func(t *testing.T) {
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

		suc := NewSystemUserCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 1)
		require.NotNil(t, ctr[host].Error)
	})
}

// Helper function to create a dummy server
func createDummyServer(requiredStatusCode int) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

			if r.URL.Path == constants.SYSTEM_USER_CHECK_API_PATH {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(systemUser))
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
