package fiberutils

import (
	"reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

func TestGetHostFormEndPoint(t *testing.T) {
	type args struct {
		endpoint string
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		{
			name: "valid_http_endpoint",
			args: args{
				endpoint: "http://127.0.0.1:8080",
			},
			want:    "127.0.0.1",
			wantErr: false,
		},
		{
			name: "valid_https_endpoint",
			args: args{
				endpoint: "https://127.0.0.1:8443",
			},
			want:    "127.0.0.1",
			wantErr: false,
		},
		{
			name: "missing_scheme",
			args: args{
				endpoint: "127.0.0.1:8080",
			},
			want:    "",
			wantErr: true,
		},
		{
			name: "invalid_endpoint",
			args: args{
				endpoint: "foo",
			},
			want:    "",
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetHostFormEndPoint(tt.args.endpoint)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetHostFormEndPoint() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("GetHostFormEndPoint() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestIPNodeMap(t *testing.T) {
	// Test case 1: Valid input
	var config models.Config
	config.Hardware.AutomateNodeIps = []string{"10.0.0.1", "10.0.0.2"}
	config.Hardware.ChefInfraServerNodeIps = []string{"10.0.0.3", "10.0.0.4"}
	config.Hardware.PostgresqlNodeIps = []string{"10.0.0.5", "10.0.0.6"}
	config.Hardware.OpenSearchNodeIps = []string{"10.0.0.7", "10.0.0.8"}

	result := ConstructIpAndNodeTypeMap(config)
	expected := map[string]string{
		"10.0.0.1": "automate",
		"10.0.0.2": "automate",
		"10.0.0.3": "chef-infra-server",
		"10.0.0.4": "chef-infra-server",
		"10.0.0.5": "postgresql",
		"10.0.0.6": "postgresql",
		"10.0.0.7": "opensearch",
		"10.0.0.8": "opensearch",
	}
	if !reflect.DeepEqual(result, expected) {
		t.Errorf("Test case 1 failed: Expected %v but got %v", expected, result)
	}

	// Test case 2: Empty input
	config = models.Config{}
	result = ConstructIpAndNodeTypeMap(config)
	expected = map[string]string{}
	if !reflect.DeepEqual(result, expected) {
		t.Errorf("Test case 2 failed: Expected %v but got %v", expected, result)
	}
}
