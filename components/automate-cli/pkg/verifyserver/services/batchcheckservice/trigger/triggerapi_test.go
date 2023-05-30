package trigger

import (
	"encoding/json"
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
	softwareVersionResp = `{
		"status": "success",
		"result": {
			"passed": true,
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

	osResourceCheck = `{
		"status": "SUCCESS",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "OS CPU count check",
					"passed": true,
					"success_msg": "CPU count is >= 4",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "OS CPU speed check",
					"passed": true,
					"success_msg": "CPU speed should be >= 2Ghz",
					"error_msg": "",
					"resolution_msg": ""
				}
			]
		}
	}`

	pgResourceCheck = `{
		"status": "PASSED",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "PG CPU count check",
					"passed": true,
					"success_msg": "CPU count is >= 4",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "PG CPU speed check",
					"passed": true,
					"success_msg": "CPU speed should be >= 2Ghz",
					"error_msg": "",
					"resolution_msg": ""
				}
			]
		}
	}`

	automateResourceCheck = `{
		"status": "PASSED",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "Automate CPU count check",
					"passed": true,
					"success_msg": "CPU count is >= 4",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "Automate CPU speed check",
					"passed": true,
					"success_msg": "CPU speed should be >= 2Ghz",
					"error_msg": "",
					"resolution_msg": ""
				}
			]
		}
	}`

	bastionResourceCheck = `{
		"status": "PASSED",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "Bastion CPU count check",
					"passed": true,
					"success_msg": "CPU count is >= 4",
					"error_msg": "",
					"resolution_msg": ""
				},
				{
					"title": "Bastion CPU speed check",
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
		go TriggerCheckAPI(server.URL+constants.SOFTWARE_VERSION_CHECK_API_PATH, server.URL, "automate", http.MethodGet, output, nil)

		// Wait for the response
		response := <-output
		// Assert the expected response
		require.NotNil(t, response)
		require.Equal(t, "success", response.Status)
		require.Equal(t, "", response.Result.Message)
		require.Equal(t, "", response.Result.Check)
		require.Equal(t, "automate", response.NodeType)
		require.True(t, response.Result.Passed)
	})
	t.Run("Endpoint not reachable", func(t *testing.T) {
		endPoint := "http://nonexistent-api.com"
		host := "example.com"
		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go TriggerCheckAPI(endPoint, host, constants.POSTGRESQL, http.MethodGet, output, nil)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Result.Error)
		assert.Equal(t, http.StatusInternalServerError, response.Result.Error.Code)
		assert.Contains(t, response.Result.Error.Message, `"http://nonexistent-api.com": dial tcp: lookup nonexistent-api.com`)
		require.Equal(t, "postgresql", response.NodeType)

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
		go TriggerCheckAPI(server.URL+constants.SOFTWARE_VERSION_CHECK_API_PATH, server.URL, constants.AUTOMATE, http.MethodGet, output, nil)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Result.Error)
		assert.Equal(t, http.StatusBadRequest, response.Result.Error.Code)
		assert.Equal(t, "error while connecting to the endpoint, received invalid status code", response.Result.Error.Message)
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
		go TriggerCheckAPI(server.URL+constants.SOFTWARE_VERSION_CHECK_API_PATH, server.URL, constants.AUTOMATE, http.MethodGet, output, nil)

		// Wait for the response
		response := <-output

		// Assert the expected error response
		require.NotNil(t, response.Result.Error)
		assert.Equal(t, http.StatusInternalServerError, response.Result.Error.Code)
		assert.Equal(t, `error while parsing the response data:invalid character 'i' looking for beginning of object key string`, response.Result.Error.Message)
	})

	t.Run("Request creation error", func(t *testing.T) {
		endPoint := "http://example.com/api/v1/checks/software-versions"
		host := "example.com"
		output := make(chan models.CheckTriggerResponse)

		// Call the function under test
		go TriggerCheckAPI(endPoint, host, constants.AUTOMATE, http.MethodGet, output, nil)

		// Wait for the response
		response := <-output
		// Assert the expected error response
		require.NotNil(t, response.Result.Error)
		require.Equal(t, http.StatusNotFound, response.Result.Error.Code)
		assert.Equal(t, "error while connecting to the endpoint, received invalid status code", response.Result.Error.Message)
	})
	t.Run("Invalid Request Body", func(t *testing.T) {
		endPoint := "http://example.com/api/v1/checks/software-versions"
		host := "example.com"
		output := make(chan models.CheckTriggerResponse)
		reqBody := make(chan int) //invalid request

		// Call the function under test
		go TriggerCheckAPI(endPoint, host, constants.AUTOMATE, http.MethodGet, output, reqBody)

		// Wait for the response
		response := <-output
		// Assert the expected error response
		require.NotNil(t, response.Result.Error)
		require.Equal(t, http.StatusBadRequest, response.Result.Error.Code)
		assert.Equal(t, "error while reading the request body: json: unsupported type: chan int", response.Result.Error.Message)
	})

}

func Test_RunCheck(t *testing.T) {
	t.Run("Software Version Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer()
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				ChefInfraServerNodeCount: 1,
				ChefInfraServerNodeIps:   []string{host},
			},
		}
		log := logger.NewLogrusStandardLogger()

		path := constants.SOFTWARE_VERSION_CHECK_API_PATH

		// Call the function being tested

		result := RunCheck(config, log, port, path, "")
		require.Equal(t, 2, len(result))
		require.NotNil(t, result)
		require.Nil(t, result[0].Result.Error)
		require.Len(t, result[0].Result.Checks, 2)
	})

	t.Run("System Resource Check - Automate", func(t *testing.T) {
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
		require.Nil(t, result[0].Result.Error)
		require.Len(t, result[0].Result.Checks, 2)
		require.Equal(t, result[0].Status, "PASSED")
	})

	t.Run("System Resource Check - PostgreSQL, OpenSearch", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer()
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				PostgresqlNodeCount: 1,
				PostgresqlNodeIps:   []string{host},
				OpenSearchNodeCount: 1,
				OpenSearchNodeIps:   []string{host},
			},
		}
		log := logger.NewLogrusStandardLogger()

		path := constants.SYSTEM_RESOURCE_CHECK_API_PATH
		depState := "your_deployment_state"

		// Call the function being tested
		results := RunCheck(config, log, port, path, depState)
		for _, result := range results {
			if result.NodeType == constants.BASTION {
				require.Equal(t, "PASSED", result.Status)
				require.Empty(t, result.Result.Message)
				require.True(t, result.Result.Passed)

				resp1 := result.Result.Checks[0]
				require.Equal(t, "Bastion CPU count check", resp1.Title)
				require.Equal(t, "CPU count is >= 4", resp1.SuccessMsg)
				require.Empty(t, resp1.ResolutionMsg)
				require.Empty(t, resp1.ErrorMsg)
				require.True(t, resp1.Passed)

				resp2 := result.Result.Checks[1]
				require.Equal(t, "Bastion CPU speed check", resp2.Title)
				require.Equal(t, "CPU speed should be >= 2Ghz", resp2.SuccessMsg)
				require.Empty(t, resp2.ResolutionMsg)
				require.Empty(t, resp2.ErrorMsg)
				require.True(t, resp2.Passed)
			}

			if result.NodeType == constants.OPENSEARCH {
				require.Equal(t, "SUCCESS", result.Status)
				require.Empty(t, result.Result.Message)
				require.True(t, result.Result.Passed)

				resp1 := result.Result.Checks[0]
				require.Equal(t, "OS CPU count check", resp1.Title)
				require.Equal(t, "CPU count is >= 4", resp1.SuccessMsg)
				require.Empty(t, resp1.ResolutionMsg)
				require.Empty(t, resp1.ErrorMsg)
				require.True(t, resp1.Passed)

				resp2 := result.Result.Checks[1]
				require.Equal(t, "OS CPU speed check", resp2.Title)
				require.Equal(t, "CPU speed should be >= 2Ghz", resp2.SuccessMsg)
				require.Empty(t, resp2.ResolutionMsg)
				require.Empty(t, resp2.ErrorMsg)
				require.True(t, resp2.Passed)
			}
			if result.NodeType == constants.POSTGRESQL {
				require.Equal(t, "PASSED", result.Status)
				require.Empty(t, result.Result.Message)
				require.True(t, result.Result.Passed)

				resp1 := result.Result.Checks[0]
				require.Equal(t, "PG CPU count check", resp1.Title)
				require.Equal(t, "CPU count is >= 4", resp1.SuccessMsg)
				require.Empty(t, resp1.ResolutionMsg)
				require.Empty(t, resp1.ErrorMsg)
				require.True(t, resp1.Passed)

				resp2 := result.Result.Checks[1]
				require.Equal(t, "PG CPU speed check", resp2.Title)
				require.Equal(t, "CPU speed should be >= 2Ghz", resp2.SuccessMsg)
				require.Empty(t, resp2.ResolutionMsg)
				require.Empty(t, resp2.ErrorMsg)
				require.True(t, resp2.Passed)
			}
		}
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
		require.Len(t, result, 1)
		require.NotNil(t, result)
		require.Nil(t, result[0].Result.Error)
		require.Len(t, result[0].Result.Checks, 3)
		require.Equal(t, result[0].Status, "SUCCESS")
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

			reqParameters := r.URL.Query()
			nodeType := reqParameters.Get("node_type")
			if nodeType == constants.OPENSEARCH {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(osResourceCheck))
			}
			if nodeType == constants.POSTGRESQL {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(pgResourceCheck))
			}
			if nodeType == constants.BASTION {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(bastionResourceCheck))
			}

			if nodeType == constants.AUTOMATE {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte(automateResourceCheck))
			}
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
