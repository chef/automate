package softwareversionchecktrigger

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

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

func TestSoftwareVersionCheck_Run(t *testing.T) {
	t.Run("Software Version Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusOK, "")
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

		suc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		// Assert the expected result
		require.Len(t, ctr, 2)
		require.Nil(t, ctr[0].Result.Error)
		require.Len(t, ctr[0].Result.Checks, 2)
		require.Equal(t, "API check", ctr[0].Result.Check)

		checkResp := ctr[0].Result.Checks[1]

		assert.Equal(t, "Check 2", checkResp.Title)
		assert.Equal(t, false, checkResp.Passed)
		assert.Equal(t, "", checkResp.SuccessMsg)
		assert.Equal(t, "Check 2 failed", checkResp.ErrorMsg)
		assert.Equal(t, "Please check the configuration", checkResp.ResolutionMsg)

	})

	t.Run("Failed Software Version Check", func(t *testing.T) {
		// Create a dummy server
		requiredStatusResponse := `{"error":{"code":500,"message":"error while connecting to the endpoint: endpoint not found"}}`
		server, host, port := createDummyServer(t, http.StatusInternalServerError, requiredStatusResponse)
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

		suc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 2)
		require.NotNil(t, ctr[0].Result.Error)
		require.Equal(t, ctr[0].Result.Error.Code, http.StatusInternalServerError)
		assert.Equal(t, "error while connecting to the endpoint: endpoint not found", ctr[0].Result.Error.Error())
	})

	t.Run("Nil Hardware", func(t *testing.T) {
		// Create a dummy server
		server, _, port := createDummyServer(t, http.StatusInternalServerError, "")
		defer server.Close()

		// Test data
		config := &models.Config{
			Hardware: nil,
		}

		suc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), port)
		ctr := suc.Run(config)

		require.Len(t, ctr, 5)
		for _, v := range ctr {
			if v.NodeType == constants.BASTION {
				assert.Equal(t, constants.LOCALHOST, v.Host)
			}
			assert.True(t, v.Result.Skipped)
			assert.Equal(t, constants.SOFTWARE_VERSIONS, v.CheckType)
			assert.Equal(t, constants.SOFTWARE_VERSIONS, v.Result.Check)
		}
	})

}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			assert.Equal(t, constants.SOFTWARE_VERSION_CHECK_API_PATH, r.URL.Path)
			reqParameters := r.URL.Query()
			assert.Equal(t, 1, len(reqParameters))
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
		w.Write([]byte(requiredStatusResponse))
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewSoftwareVersionCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}
