package hardwareresourcechecktrigger

import (
	"encoding/json"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
)

const (
	hardwareCheckResp = `{
		"status": "SUCCESS",
		"result": [
		  {
			"ip": "1.2.3.4",
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
			"ip": "5.6.7.8",
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
			"ip": "10.11.12.13",
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
			"ip": "14.15.16.17",
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
				"passed": false,
				"success_msg": "",
				"error_msg": "OpenSearch Type has invalid count as per Automate HA requirement",
          		"resolution_msg": "Hardware Resource Count for OpenSearch Type should be according to Automate HA requirements"
			  }
			]
		  }
		]
	  }`
)

func GetRequestJson() models.Config {
	ipConfig := models.Config{}

	json.Unmarshal([]byte(`{
		  "ssh_user": {
			"user_name": "ubuntu",
			"private_key": "test_key",
			"sudo_password": "test@123"
		  },
		  "arch": "existing_nodes",
		  "backup": {
			"file_system": {
			  "mount_location": "/mnt/automate_backups"
			}
		  },
		  "hardware": {
			"automate_node_count": 1,
			"automate_node_ips": [
			  "1.2.3.4"
			],
			"chef_infra_server_node_count": 1,
			"chef_infra_server_node_ips": [
			  "5.6.7.8"
			],
			"postgresql_node_count": 1,
			"postgresql_node_ips": [
			  "9.10.11.12"
			],
			"opensearch_node_count": 1,
			"opensearch_node_ips": [
			  "14.15.16.17"
			]
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
	testPort := "1234"
	hrc := NewHardwareResourceCountCheck(logger.NewTestLogger(), testPort)
	assert.NotNil(t, hrc)
	assert.NotNil(t, hrc.log)
	assert.Equal(t, constants.LOCAL_HOST_URL, hrc.host)
	assert.Equal(t, testPort, hrc.port)
}

func startMockServerOnCustomPort(mockServer *httptest.Server, port string) error {
	l, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%s", port))
	if err != nil {
		return err
	}
	mockServer.Listener = l
	mockServer.Start()
	return nil
}

func TestHardwareResourceCountCheck_Run(t *testing.T) {

	t.Run("Returns OK", func(t *testing.T) {
		//starting the mock server on custom port
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(hardwareCheckResp))
		}))
		err := startMockServerOnCustomPort(mockServer, "1220")
		assert.NoError(t, err)
		defer mockServer.Close()

		hrc := NewHardwareResourceCountCheck(logger.NewTestLogger(), "1220")
		request := GetRequestJson()
		finalResp := hrc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(finalResp))

		for _, resp := range finalResp {
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT, resp.Result.Check)
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT_MSG, resp.Result.Message)
			assert.NotEmpty(t, resp.Result.Checks)
			if resp.Host == "14.15.16.17" {
				triggerResp := resp
				assert.Equal(t, "SUCCESS", triggerResp.Status)
				assert.Nil(t, triggerResp.Result.Error)
				assert.NotEmpty(t, triggerResp.Result)
				assert.Equal(t, constants.OPENSEARCH, triggerResp.NodeType)
				assert.Equal(t, resp.Result.Passed, false)
				assert.Equal(t, 4, len(triggerResp.Result.Checks))
				assert.Equal(t, "IP address", triggerResp.Result.Checks[0].Title)
				assert.Equal(t, true, triggerResp.Result.Checks[0].Passed)
				assert.Equal(t, "IP address is unique", triggerResp.Result.Checks[0].SuccessMsg)
				assert.Equal(t, "OpenSearch Type has invalid count as per Automate HA requirement", triggerResp.Result.Checks[3].ErrorMsg)
				assert.Equal(t, "Hardware Resource Count for OpenSearch Type should be according to Automate HA requirements", triggerResp.Result.Checks[3].ResolutionMsg)
			} else {
				assert.Equal(t, resp.Result.Passed, true)
			}
		}
	})

	t.Run("Returns error", func(t *testing.T) {
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
			w.Write([]byte(`invalid JSON`))
		}))
		err := startMockServerOnCustomPort(mockServer, "1134")
		assert.NoError(t, err)
		defer mockServer.Close()

		hrc := NewHardwareResourceCountCheck(logger.NewTestLogger(), "1134")
		request := GetRequestJson()
		mapStruct := hrc.Run(request)
		totalIps := request.Hardware.AutomateNodeCount + request.Hardware.ChefInfraServerNodeCount + request.Hardware.PostgresqlNodeCount + request.Hardware.OpenSearchNodeCount
		assert.Equal(t, totalIps, len(mapStruct))

		for _, resp := range mapStruct {
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT, resp.Result.Check)
			assert.Equal(t, constants.HARDWARE_RESOURCE_COUNT_MSG, resp.Result.Message)
			assert.NotNil(t, resp.Result.Error)
			assert.Empty(t, resp.Result.Checks)
			assert.Equal(t, resp.Result.Passed, false)
		}
	})
}

func TestHardwareResourceCountCheck_TriggerHardwareResourceCountCheck(t *testing.T) {
	t.Run("cannot reach", func(t *testing.T) {
		// create the CheckTrigger instance to be tested
		hrc := HardwareResourceCountCheck{log: logger.NewTestLogger(), host: "invalid-url"}

		// make the HTTP request to an invalid URL
		resp, err := hrc.TriggerHardwareResourceCountCheck(GetRequestJson())

		require.Error(t, err)
		require.Nil(t, resp)
	})

	t.Run("Bad request", func(t *testing.T) {
		mockServer := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusBadRequest)
		}))
		err := startMockServerOnCustomPort(mockServer, "1238")
		assert.NoError(t, err)
		defer mockServer.Close()

		hrc := NewHardwareResourceCountCheck(logger.NewTestLogger(), "1238")
		resp, err := hrc.TriggerHardwareResourceCountCheck(GetRequestJson())
		require.Error(t, err)
		require.Nil(t, resp)
	})
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewHardwareResourceCountCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}
