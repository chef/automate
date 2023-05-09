package fiberutils

import (
	"net/http"
	"reflect"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/require"
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

func Test_ipTOEndPoint(t *testing.T) {
	t.Run("test 1", func(t *testing.T) {
		got := HostToEndPoint("http://127.0.0.1", "8080", "/api/v1/checks/software-versions", "automate")
		require.NotEmpty(t, got)
		require.Equal(t, got, "http://127.0.0.1:8080/api/v1/checks/software-versions?node_type=automate")
	})
	t.Run("test 1", func(t *testing.T) {
		got := HostToEndPoint("http://127.0.0.2", "8081", "/api/v1/checks/software-versions", "")
		require.NotEmpty(t, got)
		require.Equal(t, got, "http://127.0.0.2:8081/api/v1/checks/software-versions")
	})
}

func TestSendError(t *testing.T) {
	t.Run("Add a value to the channel", func(t *testing.T) {
		endPoint := "https://localhost/abc/def"
		output := make(chan models.CheckTriggerResponse)
		ctr := models.CheckTriggerResponse{Status: "pass", Host: "localhost"}
		go SendError(endPoint, "", http.StatusOK, output, ctr)

		res := <-output
		require.NotNil(t, res)
		require.Equal(t, "pass", res.Status)
		require.Equal(t, "localhost", res.Host)
	})

	t.Run("Add an error val to the channel", func(t *testing.T) {
		endPoint := "https://localhost/abc/def"
		output := make(chan models.CheckTriggerResponse)
		ctr := models.CheckTriggerResponse{Status: "failed", Host: ""}
		go SendError(endPoint, "error occured", http.StatusNotFound, output, ctr)

		res := <-output
		require.NotNil(t, res)
		require.Equal(t, "failed", res.Status)
		require.Equal(t, &fiber.Error{Code: 404, Message: "error occured"}, res.Error)
	})
}
