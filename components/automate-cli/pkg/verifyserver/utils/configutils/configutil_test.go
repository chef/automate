package configutils

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
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
func TestGetIps(t *testing.T) {
	config := GetRequestJson()

	ips := GetIps(config)
	expected := []string{
		"1.2.3.4",
		"5.6.7.8",
		"9.10.11.12",
		"14.15.16.17",
	}
	assert.NotNil(t, ips)
	assert.Equal(t, 4, len(ips))
	assert.Equal(t, expected, ips)

}

func TestGetNodeTypeMap(t *testing.T) {

	expected := map[string][]string{
		"1.2.3.4":     []string{constants.AUTOMATE},
		"5.6.7.8":     []string{constants.CHEF_INFRA_SERVER},
		"9.10.11.12":  []string{constants.POSTGRESQL},
		"14.15.16.17": []string{constants.OPENSEARCH},
	}

	assert.Equal(t, expected, GetNodeTypeMap(GetRequestJson()))

	config := models.Config{
		Hardware: models.Hardware{
			AutomateNodeIps:        []string{"192.168.1.1"},
			ChefInfraServerNodeIps: []string{"192.168.1.1"},
			OpenSearchNodeIps:      []string{"192.168.1.3"},
			PostgresqlNodeIps:      []string{"192.168.1.4"},
		},
	}

	expected_1 := map[string][]string{
		"192.168.1.1": []string{constants.AUTOMATE, constants.CHEF_INFRA_SERVER},
		"192.168.1.3": []string{constants.OPENSEARCH},
		"192.168.1.4": []string{constants.POSTGRESQL},
	}

	assert.Equal(t, expected_1, GetNodeTypeMap(config))
}

func TestIsPassed(t *testing.T) {
	checks1 := []models.Checks{
		{Passed: true},
		{Passed: true},
		{Passed: true},
	}
	assert.True(t, IsPassed(checks1))

	checks2 := []models.Checks{
		{Passed: true},
		{Passed: false},
		{Passed: true},
	}
	assert.False(t, IsPassed(checks2))

	checks3 := []models.Checks{}
	assert.True(t, IsPassed(checks3))
}

func TestPrepareTriggerResponse_WithNoError(t *testing.T) {
	// Arrange
	resp := &models.CheckTriggerResponse{
		Status: "ok",
		Result: models.ApiResult{
			Passed:  true,
			Check:   "test check",
			Message: "test message",
			Checks:  []models.Checks{},
		},
	}
	host := "test host"
	nodeType := constants.AUTOMATE
	errorString := ""
	check := "test check"
	msg := "test message"
	isError := false

	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError)

	// Assert
	if result.Status != resp.Status {
		t.Errorf("Expected status %s, but got %s", resp.Status, result.Status)
	}
	if result.Host != host {
		t.Errorf("Expected host %s, but got %s", host, result.Host)
	}
	if result.NodeType != nodeType {
		t.Errorf("Expected node type %s, but got %s", nodeType, result.NodeType)
	}
	if result.Result.Passed != resp.Result.Passed {
		t.Errorf("Expected passed %t, but got %t", resp.Result.Passed, result.Result.Passed)
	}
	if result.Result.Check != check {
		t.Errorf("Expected check %s, but got %s", check, result.Result.Check)
	}
	if result.Result.Message != msg {
		t.Errorf("Expected message %s, but got %s", msg, result.Result.Message)
	}
	if len(result.Result.Checks) != len(resp.Result.Checks) {
		t.Errorf("Expected checks length %d, but got %d", len(resp.Result.Checks), len(result.Result.Checks))
	}
}

func TestPrepareTriggerResponse_WithError(t *testing.T) {
	// Arrange
	resp := &models.CheckTriggerResponse{}
	host := "test host"
	nodeType := constants.AUTOMATE
	errorString := "test error"
	check := "test check"
	msg := "test message"
	isError := true

	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError)

	// Assert
	if result.Status != "" {
		t.Errorf("Expected empty status, but got %s", result.Status)
	}
	if result.Host != host {
		t.Errorf("Expected host %s, but got %s", host, result.Host)
	}
	if result.NodeType != nodeType {
		t.Errorf("Expected node type %s, but got %s", nodeType, result.NodeType)
	}
	if result.Result.Passed != false {
		t.Errorf("Expected passed false, but got %t", result.Result.Passed)
	}
	if result.Result.Check != check {
		t.Errorf("Expected check %s, but got %s", check, result.Result.Check)
	}
	if result.Result.Message != msg {
		t.Errorf("Expected message %s, but got %s", msg, result.Result.Message)
	}
}
