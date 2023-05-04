package trigger

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
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

func TestCheckTrigger_TriggerHardwareResourceCountCheck(t *testing.T) {
	t.Run("cannot reach", func(t *testing.T) {
		// create the CheckTrigger instance to be tested
		hrc := NewCheckTrigger(logger.NewLogger(true))

		// make the HTTP request to an invalid URL
		resp, err := hrc.TriggerHardwareResourceCountCheck("invalid-url", GetRequestJson())

		if err != nil && !strings.Contains(err.Error(), "unsupported protocol scheme") {
			t.Errorf("unexpected error: %v", err)
		}
		if resp != nil {
			t.Errorf("unexpected response: %v", resp)
		}
	})

	t.Run("Bad request", func(t *testing.T) {
		// create the CheckTrigger instance to be tested
		hrc := &CheckTrigger{}

		// call the function being tested with an endpoint that returns an error status code
		mockServer2 := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusBadRequest)
		}))
		defer mockServer2.Close()

		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer2.URL, GetRequestJson())

		if resp != nil {
			t.Errorf("unexpected result: %v", resp)
		}
		if err != nil {
			assert.EqualError(t, err, fmt.Sprintf("error triggering the API %s: status code %d ", mockServer2.URL, http.StatusBadRequest))
		}
	})

	t.Run("Invalid JSON", func(t *testing.T) {
		hrc := &CheckTrigger{}

		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(`invalid JSON`))
		}))
		defer mockServer.Close()

		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer.URL, GetRequestJson())

		if resp != nil {
			t.Errorf("unexpected result: %v", resp)
		}
		if err != nil {
			assert.EqualError(t, err, fmt.Sprintf("error decoding response from the API %s: invalid character 'i' looking for beginning of value ", mockServer.URL))
		}
	})

	t.Run("Returns OK", func(t *testing.T) {

		hrc := &CheckTrigger{}

		mockServer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(hardwareCheckResp))
		}))
		defer mockServer.Close()

		// call the function being tested
		resp, err := hrc.TriggerHardwareResourceCountCheck(mockServer.URL, GetRequestJson())

		if resp != nil {
			t.Logf("Success response : %v", resp)
		}
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
	})

}
