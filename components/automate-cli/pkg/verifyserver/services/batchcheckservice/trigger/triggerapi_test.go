package trigger

import (
	"encoding/json"
	"fmt"
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
					"title": "Group creation/validation check",
					"passed": true,
					"success_msg": "Group is created or found successfully",
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

func TestTriggerCheckAPI(t *testing.T) {
	t.Run("Passed", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Define the response based on the test case
			w.WriteHeader(http.StatusOK)
			err := json.NewEncoder(w).Encode(json.RawMessage(softwareVersionResp))
			require.NoError(t, err)
		}))
		defer server.Close()

		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go triggerCheckAPI(server.URL+"/api/v1/checks/software-versions", server.URL, output)

		// Wait for the response
		response := <-output
		fmt.Printf("response: %+v\n", response)
		// Assert the expected response
		require.NotNil(t, response)
	})
	t.Run("Endpoint not reachable", func(t *testing.T) {
		endPoint := "http://nonexistent-api.com"
		host := "example.com"
		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go triggerCheckAPI(endPoint, host, output)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Error)
		assert.Equal(t, http.StatusInternalServerError, response.Error.Code)
		assert.Equal(t, `error while connecting to the endpoint:Get "http://nonexistent-api.com": dial tcp: lookup nonexistent-api.com: no such host`, response.Error.Message)
	})
	t.Run("Non-OK status code", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Return a non-OK status code
			w.WriteHeader(http.StatusBadRequest)
		}))
		defer server.Close()

		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go triggerCheckAPI(server.URL+"/api/v1/checks/software-versions", server.URL, output)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Error)
		assert.Equal(t, http.StatusBadRequest, response.Error.Code)
		assert.Equal(t, "error while connecting to the endpoint, received invalid status code", response.Error.Message)
	})

	t.Run("Error decoding response JSON", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Return a valid status code but an invalid JSON response
			w.WriteHeader(http.StatusOK)
			w.Write([]byte("{ invalid json }"))
		}))
		defer server.Close()

		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go triggerCheckAPI(server.URL+"/api/v1/checks/software-versions", server.URL, output)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Error)
		assert.Equal(t, http.StatusInternalServerError, response.Error.Code)
		assert.Equal(t, `error while parsing the response data:invalid character 'i' looking for beginning of object key string`, response.Error.Message)
	})
}

func TestRunCheck(t *testing.T) {
	t.Run("Software Version Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer()
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}
		log := logger.NewLogrusStandardLogger()

		path := constants.SOFTWARE_VERSION_CHECK_API_PATH

		// Call the function being tested
		result := RunCheck(config, log, port, path, "")
		require.NotNil(t, result)
		require.Nil(t, result[host].Error)
		require.Len(t, result[host].Result.Checks, 2)
	})

	t.Run("System Resource Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer()
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}
		log := logger.NewLogrusStandardLogger()

		path := constants.SYSTEM_RESOURCE_CHECK_API_PATH
		depState := "your_deployment_state"

		// Call the function being tested
		result := RunCheck(config, log, port, path, depState)
		require.NotNil(t, result)
		require.Nil(t, result[host].Error)
		require.Len(t, result[host].Result.Checks, 2)
		require.Equal(t, result[host].Status, "SUCCESS")
	})

	t.Run("System User Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer()
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount: 1,
				AutomateNodeIps:   []string{host},
			},
		}
		log := logger.NewLogrusStandardLogger()

		path := constants.SYSTEM_USER_CHECK_API_PATH
		depState := ""

		// Call the function being tested
		result := RunCheck(config, log, port, path, depState)
		require.NotNil(t, result)
		require.Nil(t, result[host].Error)
		require.Len(t, result[host].Result.Checks, 3)
		require.Equal(t, result[host].Status, "SUCCESS")
	})
}

// Helper function to create a dummy server
func createDummyServer() (*httptest.Server, string, string) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == constants.SOFTWARE_VERSION_CHECK_API_PATH {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(softwareVersionResp))
		}
		if r.URL.Path == constants.SYSTEM_RESOURCE_CHECK_API_PATH {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(resourceCheck))
		}

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
