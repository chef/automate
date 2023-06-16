package systemresourcechecktrigger

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
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

var externalOS = &models.ExternalOS{
	OSDomainName:   "example.com",
	OSDomainURL:    "https://example.com",
	OSUsername:     "username",
	OSUserPassword: "password",
	OSCert:         "certificate",
	OSRoleArn:      "arn:aws:iam::123456789012:role/MyRole",
}

var externalPG = &models.ExternalPG{
	PGInstanceURL:       "http://example.com",
	PGSuperuserName:     "superuser",
	PGSuperuserPassword: "superpassword",
	PGDbUserName:        "dbuser",
	PGDbUserPassword:    "dbpassword",
	PGRootCert:          "rootcert",
}

func TestSystemResourceCheck_Run(t *testing.T) {
	t.Run("System Resource Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusOK)
		defer server.Close()

		// Test data
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
			DeploymentState: "pre-release",
			ExternalOS:      externalOS,
			ExternalPG:      externalPG,
		}

		suc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		// Assert the expected result
		require.Len(t, ctr, 2)
		require.Nil(t, ctr[0].Result.Error)
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
		config := &models.Config{
			Hardware: &models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
			ExternalOS: externalOS,
			ExternalPG: externalPG,
		}

		suc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 2)
		require.NotNil(t, ctr[0].Result.Error)
		require.Equal(t, ctr[0].Result.Error.Code, http.StatusInternalServerError)
		require.Equal(t, "error while connecting to the endpoint, received invalid status code", ctr[0].Result.Error.Error())
	})

	t.Run("Nil Hardware", func(t *testing.T) {
		// Create a dummy server
		server, _, port := createDummyServer(t, http.StatusInternalServerError)
		defer server.Close()

		// Test data
		config := &models.Config{
			Hardware:   nil,
			ExternalOS: externalOS,
			ExternalPG: &models.ExternalPG{},
		}

		suc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), port)
		got := suc.Run(config)

		fmt.Printf("ctr: %+v\n", got)
		require.Len(t, got, 5)
		assert.Equal(t, constants.UNKNONHOST, got[0].Host)
		assert.Equal(t, constants.CHEF_INFRA_SERVER, got[1].NodeType)
		assert.Equal(t, constants.SYSTEM_RESOURCES, got[1].CheckType)
		assert.True(t, got[0].Result.Skipped)
	})

}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			assert.Equal(t, constants.SYSTEM_RESOURCE_CHECK_API_PATH, r.URL.Path)
			reqParameters := r.URL.Query()
			assert.Equal(t, 2, len(reqParameters))
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

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewSystemResourceCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}
