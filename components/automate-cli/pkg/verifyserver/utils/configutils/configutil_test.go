package configutils

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
)

func GetRequestJson() models.Hardware {
	ipConfig := models.Hardware{}

	json.Unmarshal([]byte(` {
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
		`), &ipConfig)
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
		"1.2.3.4":     {constants.AUTOMATE},
		"5.6.7.8":     {constants.CHEF_INFRA_SERVER},
		"9.10.11.12":  {constants.POSTGRESQL},
		"14.15.16.17": {constants.OPENSEARCH},
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

	expectedNew := map[string][]string{
		"192.168.1.1": {constants.AUTOMATE, constants.CHEF_INFRA_SERVER},
		"192.168.1.3": {constants.OPENSEARCH},
		"192.168.1.4": {constants.POSTGRESQL},
	}

	assert.Equal(t, expectedNew, GetNodeTypeMap(config.Hardware))
}
