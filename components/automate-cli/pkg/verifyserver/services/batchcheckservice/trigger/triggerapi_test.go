package trigger

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
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
		assert.Equal(t, "error while connecting to the endpoint", response.Error.Message)
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
		assert.Equal(t, "error while connecting to the endpoint", response.Error.Message)
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
		assert.Equal(t, "error while parsing the response data", response.Error.Message)
	})

}

func TestRunCheck(t *testing.T) {

	t.Run("Software Version Check", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Define the response based on the test case
			w.WriteHeader(http.StatusOK)
			err := json.NewEncoder(w).Encode(json.RawMessage(softwareVersionResp))
			require.NoError(t, err)
		}))
		defer server.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount:   1,
				AutomateNodeIps:     []string{server.URL},
				PostgresqlNodeCount: 1,
				PostgresqlNodeIps:   []string{"127.0.0.1"},
			},
		}

		// Call the function under test
		result := RunCheck(config, logger.NewLogrusStandardLogger(), "", constants.SOFTWARE_VERSION_CHECK_API_PATH, "")
		fmt.Printf("Tese check: %+v\n", result[server.URL].Error)
		// Assert the expected result
		require.Len(t, result, 2) // Modify the count based on your configuration
		require.Nil(t, result[server.URL].Error)
		require.Error(t, result["127.0.0.1"].Error)
	})

	t.Run("System Resource Check", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Define the response based on the test case
			w.WriteHeader(http.StatusOK)
			err := json.NewEncoder(w).Encode(json.RawMessage(softwareVersionResp))
			require.NoError(t, err)
		}))
		defer server.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount:   1,
				AutomateNodeIps:     []string{server.URL},
				PostgresqlNodeCount: 1,
				PostgresqlNodeIps:   []string{"127.0.0.1"},
			},
		}

		// Call the function under test
		result := RunCheck(config, logger.NewLogrusStandardLogger(), "", constants.SYSTEM_RESOURCE_CHECK_API_PATH, "pre-deploy")

		// Assert the expected result
		require.Len(t, result, 2) // Modify the count based on your configuration
		require.Nil(t, result[server.URL].Error)
		require.Error(t, result["127.0.0.1"].Error)
	})

	t.Run("System User Check", func(t *testing.T) {
		// Create a test server to mock the API endpoint
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Define the response based on the test case
			w.WriteHeader(http.StatusOK)
			err := json.NewEncoder(w).Encode(json.RawMessage(softwareVersionResp))
			require.NoError(t, err)
		}))
		defer server.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeCount:   1,
				AutomateNodeIps:     []string{server.URL},
				PostgresqlNodeCount: 1,
				PostgresqlNodeIps:   []string{"127.0.0.1"},
			},
		}

		// Call the function under test
		result := RunCheck(config, logger.NewLogrusStandardLogger(), "", constants.SYSTEM_USER_CHECK_API_PATH, "")

		// Assert the expected result
		require.Len(t, result, 2) // Modify the count based on your configuration
		require.Nil(t, result[server.URL].Error)
		require.Equal(t, result[server.URL].Result.Passed, true)
		require.Error(t, result["127.0.0.1"].Error)
	})
}
