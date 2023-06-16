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
					"passeß": true,
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
		assert.Contains(t, response.Result.Error.Message, `error while connecting to the endpoint`)
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
		require.Equal(t, http.StatusInternalServerError, response.Result.Error.Code)
		assert.Contains(t, response.Result.Error.Message, "error while connecting to the endpoint")
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
		config := &models.Config{
			Hardware: &models.Hardware{
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
		config := &models.Config{
			Hardware: &models.Hardware{
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
		config := &models.Config{
			Hardware: &models.Hardware{
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
		config := &models.Config{
			Hardware: &models.Hardware{
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

func TestCheckEmptyOrNilExternalConfig(t *testing.T) {
	hardware := &models.Hardware{
		AutomateNodeIps:        []string{"192.168.0.1", "192.168.0.2"},
		ChefInfraServerNodeIps: []string{"192.168.0.3"},
		OpenSearchNodeIps:      []string{"192.168.0.4", "192.168.0.5", "192.168.0.6"},
		PostgresqlNodeIps:      []string{"192.168.0.7"},
	}
	externalOS := &models.ExternalOS{
		OSDomainName:   "example.com",
		OSDomainURL:    "https://example.com",
		OSUsername:     "username",
		OSUserPassword: "password",
		OSCert:         "certificate",
		OSRoleArn:      "arn:aws:iam::123456789012:role/MyRole",
	}

	externalPG := &models.ExternalPG{
		PGInstanceURL:       "http://example.com",
		PGSuperuserName:     "superuser",
		PGSuperuserPassword: "superpassword",
		PGDbUserName:        "dbuser",
		PGDbUserPassword:    "dbpassword",
		PGRootCert:          "rootcert",
	}

	t.Run("Test case 1: ExternalOS nil", func(t *testing.T) {
		//
		config := &models.Config{
			Hardware:   hardware,
			ExternalOS: nil,
			ExternalPG: externalPG,
		}

		response, isEmpty := CheckEmptyOrNilExternalConfig(config)

		// Verify the response
		assert.True(t, isEmpty, "Expected isEmpty to be true")
		assert.NotEmpty(t, response, "Expected response to be not empty")
	})

	t.Run("Test case 2: ExternalPg is nil", func(t *testing.T) {

		config := &models.Config{
			Hardware:   hardware,
			ExternalOS: externalOS,
			ExternalPG: nil,
		}

		response, isEmpty := CheckEmptyOrNilExternalConfig(config)

		// Verify the response
		assert.True(t, isEmpty, "Expected isEmpty to be true")
		assert.NotEmpty(t, response, "Expected response to be not empty")
	})

	t.Run("Test case 3: ExternalOS is empty", func(t *testing.T) {
		config := &models.Config{
			Hardware:   hardware,
			ExternalOS: &models.ExternalOS{},
			ExternalPG: externalPG,
		}

		response, isEmpty := CheckEmptyOrNilExternalConfig(config)

		// Verify the response
		assert.True(t, isEmpty, "Expected isEmpty to be true")
		assert.NotEmpty(t, response, "Expected response to be not empty")
	})

	t.Run("Test case 4: ExternalPG is empty", func(t *testing.T) {
		config := &models.Config{
			Hardware:   hardware,
			ExternalOS: externalOS,
			ExternalPG: &models.ExternalPG{},
		}

		response, isEmpty := CheckEmptyOrNilExternalConfig(config)

		// Verify the response
		assert.True(t, isEmpty, "Expected isEmpty to be true")
		assert.NotEmpty(t, response, "Expected response to be not empty")
	})

	t.Run("", func(t *testing.T) {
		// Test case 5: ExternalOS and ExternalPG are not empty or nil
		config := &models.Config{
			Hardware:   hardware,
			ExternalOS: externalOS,
			ExternalPG: externalPG,
		}

		response, isEmpty := CheckEmptyOrNilExternalConfig(config)

		// Verify the response
		assert.False(t, isEmpty, "Expected isEmpty to be false")
		assert.Nil(t, response, "Expected response to be nil")
	})
}

func TestExternalOSPGNillResp(t *testing.T) {
	// Test case 1: No hardware IPs in the config
	config := &models.Config{
		Hardware: &models.Hardware{
			AutomateNodeIps:        nil,
			ChefInfraServerNodeIps: nil,
			OpenSearchNodeIps:      nil,
			PostgresqlNodeIps:      nil,
		},
	}

	response := ExternalOSPGNillResp(config)

	// Verify the number of responses
	assert.Len(t, response, 1, "Expected one CheckTriggerResponse")

	// Verify the response content
	expectedResponse := createNilResponse("127.0.0.1", constants.BASTION)
	assert.Equal(t, expectedResponse, response[0], "Unexpected CheckTriggerResponse")

	// Test case 2: Hardware IPs present in the config
	config = &models.Config{
		Hardware: &models.Hardware{
			AutomateNodeIps:        []string{"192.168.0.1", "192.168.0.2"},
			ChefInfraServerNodeIps: []string{"192.168.0.3"},
			OpenSearchNodeIps:      []string{"192.168.0.4", "192.168.0.5", "192.168.0.6"},
			PostgresqlNodeIps:      []string{"192.168.0.7"},
		},
	}

	response = ExternalOSPGNillResp(config)

	// Verify the number of responses
	expectedCount := len(config.Hardware.AutomateNodeIps) + len(config.Hardware.ChefInfraServerNodeIps) +
		len(config.Hardware.OpenSearchNodeIps) + len(config.Hardware.PostgresqlNodeIps)
	assert.Len(t, response, expectedCount, "Unexpected number of CheckTriggerResponses")

	// Verify the response content for each hardware IP
	for _, ip := range config.Hardware.AutomateNodeIps {
		expectedResponse = createNilResponse(ip, constants.AUTOMATE)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		expectedResponse = createNilResponse(ip, constants.CHEF_INFRA_SERVER)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.OpenSearchNodeIps {
		expectedResponse = createNilResponse(ip, constants.OPENSEARCH)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.PostgresqlNodeIps {
		expectedResponse = createNilResponse(ip, constants.POSTGRESQL)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}
}

func TestCreateNilResponse(t *testing.T) {
	host := "example.com"
	nodeType := "example"

	response := createNilResponse(host, nodeType)

	// Verify the values of the returned struct
	assert.Equal(t, constants.AUTOMATE, response.NodeType, "Unexpected NodeType value")
	assert.True(t, response.Result.Skipped, "Expected Skipped to be true")
	assert.Equal(t, host, response.Host, "Unexpected Host value")
}

func TestExternalOSPGEmptyResp(t *testing.T) {
	// Test case 1: No hardware IPs in the config
	config := &models.Config{
		Hardware: &models.Hardware{
			AutomateNodeIps:        nil,
			ChefInfraServerNodeIps: nil,
			OpenSearchNodeIps:      nil,
			PostgresqlNodeIps:      nil,
		},
	}

	response := ExternalOSPGEmptyResp(config)

	// Verify the number of responses
	assert.Len(t, response, 1, "Expected one CheckTriggerResponse")

	// Verify the response content
	expectedResponse := createErrorResponse("127.0.0.1", constants.BASTION)
	assert.Equal(t, expectedResponse, response[0], "Unexpected CheckTriggerResponse")

	// Test case 2: Hardware IPs present in the config
	config = &models.Config{
		Hardware: &models.Hardware{
			AutomateNodeIps:        []string{"192.168.0.1", "192.168.0.2"},
			ChefInfraServerNodeIps: []string{"192.168.0.3"},
			OpenSearchNodeIps:      []string{"192.168.0.4", "192.168.0.5", "192.168.0.6"},
			PostgresqlNodeIps:      []string{"192.168.0.7"},
		},
	}

	response = ExternalOSPGEmptyResp(config)

	// Verify the number of responses
	expectedCount := len(config.Hardware.AutomateNodeIps) + len(config.Hardware.ChefInfraServerNodeIps) +
		len(config.Hardware.OpenSearchNodeIps) + len(config.Hardware.PostgresqlNodeIps)
	assert.Len(t, response, expectedCount, "Unexpected number of CheckTriggerResponses")

	// Verify the response content for each hardware IP
	for _, ip := range config.Hardware.AutomateNodeIps {
		expectedResponse = createErrorResponse(ip, constants.AUTOMATE)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		expectedResponse = createErrorResponse(ip, constants.CHEF_INFRA_SERVER)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.OpenSearchNodeIps {
		expectedResponse = createErrorResponse(ip, constants.OPENSEARCH)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}

	for _, ip := range config.Hardware.PostgresqlNodeIps {
		expectedResponse = createErrorResponse(ip, constants.POSTGRESQL)
		assert.Contains(t, response, expectedResponse, "Missing CheckTriggerResponse")
	}
}

func TestCreateErrorResponse(t *testing.T) {
	host := "example.com"
	nodeType := "example"

	response := createErrorResponse(host, nodeType)

	// Verify the values of the returned struct
	assert.Equal(t, host, response.Host, "Unexpected Host value")
	assert.Equal(t, nodeType, response.NodeType, "Unexpected NodeType value")
	assert.False(t, response.Result.Passed, "Expected Passed to be false")
	assert.NotNil(t, response.Result.Error, "Expected Error to be non-nil")
	assert.Equal(t, http.StatusInternalServerError, response.Result.Error.Code, "Unexpected Error Code value")
	assert.Equal(t, "External OS or PG configuration is missing", response.Result.Error.Message, "Unexpected Error Message value")
}

func TestIsEmptyExternalOS(t *testing.T) {
	// Create a test case with all fields empty
	emptyExternalOS := &models.ExternalOS{}

	// Test the true condition for each field
	assert.True(t, IsEmptyExternalOS(emptyExternalOS), "Expected true for all empty fields, but got false")

	// Test the false condition for each field
	nonEmptyExternalOS := &models.ExternalOS{
		OSDomainName:   "example.com",
		OSDomainURL:    "https://example.com",
		OSUsername:     "username",
		OSUserPassword: "password",
		OSCert:         "certificate",
		OSRoleArn:      "arn:aws:iam::123456789012:role/MyRole",
	}

	assert.False(t, IsEmptyExternalOS(nonEmptyExternalOS), "Expected false for all non-empty fields, but got true")
}

func TestIsEmptyExternalPG(t *testing.T) {
	// Create a test case with all fields empty
	emptyExternalPG := &models.ExternalPG{}

	// Test the true condition for each field
	assert.True(t, IsEmptyExternalPG(emptyExternalPG), "Expected true for all empty fields, but got false")

	// Test the false condition for each field
	nonEmptyExternalPG := &models.ExternalPG{
		PGInstanceURL:       "example.com",
		PGSuperuserName:     "superuser",
		PGSuperuserPassword: "password",
		PGDbUserName:        "dbuser",
		PGDbUserPassword:    "dbpassword",
		PGRootCert:          "certificate",
	}

	assert.False(t, IsEmptyExternalPG(nonEmptyExternalPG), "Expected false for all non-empty fields, but got true")
}
