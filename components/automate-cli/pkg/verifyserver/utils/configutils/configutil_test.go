package configutils

import (
	"encoding/json"
	"testing"

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
	assert.NotNil(t, ips)
	assert.Equal(t, 4, len(ips))

}
