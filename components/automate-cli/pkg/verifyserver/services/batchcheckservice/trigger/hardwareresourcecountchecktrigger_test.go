package trigger

import (
	"encoding/json"
	"net/http"
	reflect "reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/mocks"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/restutils"
)

func init() {
	restutils.Client = &mocks.MockClient{}
}
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

func GetResponseJson() models.CheckTriggerResponse {
	output := models.CheckTriggerResponse{}

	json.Unmarshal([]byte(`[
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
	  ]`), &output)
	return output
}
func TestCheckTrigger_HardwareResourceCountCheck(t *testing.T) {
	type fields struct {
		Logger logger.ILogger
	}
	type args struct {
		config models.Config
	}

	tests := []struct {
		name    string
		fields  fields
		args    args
		want    models.CheckTriggerResponse
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name:    "Success case",
			fields:  fields{Logger: logger.NewLogger(true)},
			args:    args{config: GetRequestJson()},
			want:    ``,
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hrc := &CheckTrigger{
				Logger: tt.fields.Logger,
			}
			got, err := hrc.HardwareResourceCountCheck(tt.args.config)
			if (err != nil) != tt.wantErr {
				t.Errorf("CheckTrigger.HardwareResourceCountCheck() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("CheckTrigger.HardwareResourceCountCheck() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_post(t *testing.T) {
	type args struct {
		url  string
		body interface{}
	}
	tests := []struct {
		name    string
		args    args
		want    *http.Response
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := post(tt.args.url, tt.args.body)
			if (err != nil) != tt.wantErr {
				t.Errorf("post() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("post() = %v, want %v", got, tt.want)
			}
		})
	}
}
