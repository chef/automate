package trigger

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const (
	hardwareCheckResp = `[
		{
		  "ip": "172.154.0.1",
		  "node_type": "automate",
		  "checks": [
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is unique",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is of valid format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "Not shared with backend nodes",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "<Node_type> Type has valid count as per Automate HA requirement",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		},
		{
		  "ip": "172.154.0.3",
		  "node_type": "chef-infra-server",
		  "checks": [
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is unique",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is of valid format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "Not shared with backend nodes",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "<Node_type> Type has valid count as per Automate HA requirement",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		},
		{
		  "ip": "172.154.0.5",
		  "node_type": "postgresql",
		  "checks": [
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is unique",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is of valid format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "Not shared with backend nodes",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "<Node_type> Type has valid count as per Automate HA requirement",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		},
		{
		  "ip": "172.154.0.8",
		  "node_type": "opensearch",
		  "checks": [
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is unique",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "IP address is of valid format",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "Not shared with backend nodes",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "IP address",
			  "passed": true,
			  "success_msg": "<Node_type> Type has valid count as per Automate HA requirement",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  ]`
)

func GetRequestJson() models.Config {
	ipConfig := models.Config{}

	json.Unmarshal([]byte(`{
		"checks": [
		  "hardware-resource-count",
		  "certificate",
		  "ssh-user"
		],
		"config": {
		  "ssh_user": {
			"user_name": "ubuntu",
			"private_key": "----- BEGIN PRIVATE RSA -----",
			"sudo_password": "test@123"
		  },
		  "arch": "existing_nodes",
		  "backup": {
			"file_system": {
			  "mount_location": "/mnt/automate_backups"
			}
		  },
		  "hardware": {
			"automate_node_count": 2,
			"automate_node_ips": [
			  "172.154.0.1",
			  "172.154.0.2"
			],
			"chef_infra_server_node_count": 2,
			"chef_infra_server_node_ips": [
			  "172.154.0.3",
			  "172.154.0.4"
			],
			"postgresql_node_count": 3,
			"postgresql_node_ips": [
			  "172.154.0.5",
			  "172.154.0.6",
			  "172.154.0.7"
			],
			"opensearch_node_count": 3,
			"opensearch_node_ips": [
			  "172.154.0.8",
			  "172.154.0.9",
			  "172.154.0.10"
			]
		  }
		}
	  }`), &ipConfig)
	return ipConfig
}

// mockTransport is a mock implementation of the http.RoundTripper interface
type mockTransport struct{}

// RoundTrip returns an error for every request
func (m *mockTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	return nil, fmt.Errorf("mock error")
}
func TestNewHardwareResourceCountCheck(t *testing.T) {
	hrc := NewHardwareResourceCountCheck()
	assert.NotNil(t, hrc)
	assert.NotNil(t, hrc.log)
}

func TestHardwareResourceCountCheck_Run(t *testing.T) {

	t.Run("Returns OK", func(t *testing.T) {
		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(hardwareCheckResp))
		}))
		defer mockServer.Close()

		hrc := NewHardwareResourceCountCheck()
		request := GetRequestJson()
		mapStruct := hrc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(mapStruct))

		for _, resp := range mapStruct {
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT, resp.Result.Check)
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT_MSG, resp.Result.Message)
			if resp.Error != nil {
				assert.Empty(t, resp.Result.Checks)
			}
		}
	})
}

func TestHardwareResourceCountCheck_TriggerHardwareResourceCountCheck(t *testing.T) {
	t.Run("cannot reach", func(t *testing.T) {
		// create the CheckTrigger instance to be tested
		hrc := NewHardwareResourceCountCheck()

		// make the HTTP request to an invalid URL
		resp, err := hrc.TriggerHardwareResourceCountCheck("invalid-url", GetRequestJson())

		require.Error(t, err)
		require.Nil(t, resp)
	})

	t.Run("Bad request", func(t *testing.T) {
		// create the CheckTrigger instance to be tested
		hrc := NewHardwareResourceCountCheck()

		// call the function being tested with an endpoint that returns an error status code
		mockServer2 := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusBadRequest)
		}))
		defer mockServer2.Close()

		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer2.URL, GetRequestJson())
		require.Error(t, err)
		require.Nil(t, resp)
	})

	t.Run("Invalid JSON", func(t *testing.T) {
		hrc := NewHardwareResourceCountCheck()

		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(`invalid JSON`))
		}))
		defer mockServer.Close()

		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer.URL, GetRequestJson())

		require.Error(t, err)
		require.Nil(t, resp)
	})

	t.Run("Returns OK", func(t *testing.T) {

		hrc := NewHardwareResourceCountCheck()

		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(hardwareCheckResp))
		}))
		defer mockServer.Close()

		// call the function being tested
		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer.URL, GetRequestJson())

		require.Nil(t, err)
		require.NotNil(t, resp)
	})

}

func TestPost(t *testing.T) {
	// Create a test server to receive requests
	server := httptest.NewServer(http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
		// Check that the request method is POST
		if req.Method != http.MethodPost {
			t.Errorf("Unexpected request method. Expected %v, got %v", http.MethodPost, req.Method)
		}
		// Check that the request body is correct
		expectedBody := "{\"org\":\"chef\"}"
		requestBody, err := ioutil.ReadAll(req.Body)
		if err != nil {
			t.Errorf("Unexpected error reading request body: %v", err)
		}
		if string(requestBody) != expectedBody {
			t.Errorf("Unexpected request body. Expected %v, got %v", expectedBody, string(requestBody))
		}
		// Write a response to the client
		rw.WriteHeader(http.StatusOK)
		rw.Write([]byte("{\"status\":\"OK\"}"))
	}))
	defer server.Close()

	// Call the Post function with a test URL and body
	url := server.URL
	body := map[string]string{"org": "chef"}
	resp, err := Post(url, body)
	if err != nil {
		t.Errorf("Unexpected error from Post function: %v", err)
	}

	// Check that the response status code is correct
	if resp.StatusCode != http.StatusOK {
		t.Errorf("Unexpected response status code. Expected %v, got %v", http.StatusOK, resp.StatusCode)
	}
	// Check that the response body is correct
	expectedResponse := "{\"status\":\"OK\"}"
	responseBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		t.Errorf("Unexpected error reading response body: %v", err)
	}
	if string(responseBody) != expectedResponse {
		t.Errorf("Unexpected response body. Expected %v, got %v", expectedResponse, string(responseBody))
	}
}
