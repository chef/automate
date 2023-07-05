package externalopensearchchecktrigger

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

const (
	externalOpensearchResponseSuccess = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Connection successfully tested",
			  "passed": true,
			  "status": "PASS",
			  "success_msg": "Machine is able to connect with External Managed OpenSeach",
			  "error_msg": "",
			  "resolution_msg": "",
			  "debug_msg": ""
			}
		  ]
		}
	  }`

	externalOpensearchResponseSuccessAutomateExpected = `[
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": true,
			  "checks": [
				{
				  "title": "Connection successfully tested",
				  "passed": true,
				  "status": "PASS",
				  "success_msg": "Machine is able to connect with External Managed OpenSeach",
				  "error_msg": "",
				  "resolution_msg": "",
				  "debug_msg": ""
				}
			  ]
			}
		},
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": true,
			  "checks": [
				{
				  "title": "Connection successfully tested",
				  "passed": true,
				  "status": "PASS",
				  "success_msg": "Machine is able to connect with External Managed OpenSeach",
				  "error_msg": "",
				  "resolution_msg": "",
				  "debug_msg": ""
				}
			  ]
			}
		}
		]
	  `

	externalOpensearchResponseSuccessChefServerExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Connection successfully tested",
			  "passed": true,
			  "status": "PASS",
			  "success_msg": "Machine is able to connect with External Managed OpenSeach",
			  "error_msg": "",
			  "resolution_msg": "",
			  "debug_msg": ""
			}
		  ]
		}
	},
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Connection successfully tested",
			  "passed": true,
			  "status": "PASS",
			  "success_msg": "Machine is able to connect with External Managed OpenSeach",
			  "error_msg": "",
			  "resolution_msg": "",
			  "debug_msg": ""
			}
		  ]
		}
	}]`
	externalOpensearchResponseFailure = `
	{
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "OpenSearch Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed OpenSeach",
			  "resolution_msg": "Ensure that the OpenSearch configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	}
	
	`
	externalOpensearchResponseFailureAutomateExpected = `[
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": false,
			  "checks": [
				{
				  "title": "OpenSearch Connection failed",
				  "passed": false,
				  "status": "PASS",
				  "success_msg": "",
				  "error_msg": "Machine is unable to connect with External Managed OpenSeach",
				  "resolution_msg": "Ensure that the OpenSearch configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
				  "debug_msg": ""
				}
			  ]
			}
		},
		{
			"status": "SUCCESS",
			"node_type" :"automate",
			"result": {
			  "passed": false,
			  "checks": [
				{
				  "title": "OpenSearch Connection failed",
				  "passed": false,
				  "status": "PASS",
				  "success_msg": "",
				  "error_msg": "Machine is unable to connect with External Managed OpenSeach",
				  "resolution_msg": "Ensure that the OpenSearch configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
				  "debug_msg": ""
				}
			  ]
			}
		}
	]`

	externalOpensearchResponseFailureChefServerExpected = `[
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "OpenSearch Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed OpenSeach",
			  "resolution_msg": "Ensure that the OpenSearch configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	},
	{
		"status": "SUCCESS",
		"node_type" :"chef-infra-server",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "OpenSearch Connection failed",
			  "passed": false,
			  "status": "PASS",
			  "success_msg": "",
			  "error_msg": "Machine is unable to connect with External Managed OpenSeach",
			  "resolution_msg": "Ensure that the OpenSearch configuration provided is correct. Review security group or firewall settings as well on the infrastructure",
			  "debug_msg": ""
			}
		  ]
		}
	}]`

	osDomainName   = "opensearch_managed"
	osDomainURL    = "https://opensearchdomain.com"
	osUsername     = "admin"
	osUserPassword = "Chefautomate"
	osCert         = "___CERT____"
	oSRoleArn      = "arn:aws:iam::123456789012:role/MyRole"
)

func getRequest() models.ExternalOSRequest {
	return models.ExternalOSRequest{
		OSDomainName:   osDomainName,
		OSDomainURL:    osDomainURL,
		OSUsername:     osUsername,
		OSUserPassword: osUserPassword,
		OSCert:         osCert,
	}

}

func TestOpensearchCheck_Run(t *testing.T) {
	type args struct {
		config *models.Config
	}

	tests := []struct {
		name               string
		isPassed           bool
		args               args
		response           string
		httpStatusCode     int
		isError            bool
		httpStatusResponse string
	}{
		{
			name:           "Opensearch Checks are passed",
			isPassed:       true,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
						AutomateNodeIps:   []string{"127.0.0.3"},
					},
					ExternalOS: &models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
						OSRoleArn:      oSRoleArn,
					},
				},
			},
			response: externalOpensearchResponseSuccessAutomateExpected,
		},

		{
			name:           "Opensearch checks are failed",
			isPassed:       false,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
						AutomateNodeIps:   []string{"127.0.0.3"},
					},
					ExternalOS: &models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
						OSRoleArn:      oSRoleArn,
					},
				},
			},
			response: externalOpensearchResponseFailureAutomateExpected,
		},
		{
			name:           "400 Bad Request",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusBadRequest,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
						AutomateNodeIps:   []string{"127.0.0.3"},
					},
					ExternalOS: &models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
						OSRoleArn:      oSRoleArn,
					},
				},
			},
			httpStatusResponse: `{"error":{"code":400, "message":"opensearch_domain_name, opensearch_domain_url, opensearch_username, opensearch_user_password or opensearch_root_cert cannot be empty"}}`,
			response: "opensearch_domain_name, opensearch_domain_url, opensearch_username, opensearch_user_password or opensearch_root_cert cannot be empty",
		},
		{
			name:           "Gateway Timeout",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusGatewayTimeout,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 2,
						AutomateNodeIps:   []string{"127.0.0.3"},
					},
					ExternalOS: &models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
						OSRoleArn:      oSRoleArn,
					},
				},
			},
			httpStatusResponse: `{"error":{"code":504,"message":"context deadline exceeded"}}`,
			response: "context deadline exceeded",
		},
		{
			name:     "Empty OS",
			isPassed: false,
			isError:  true,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount:        2,
						AutomateNodeIps:          []string{"127.0.0.1", "127.0.0.10"},
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"1.1.1.1"},
					},
					ExternalOS: &models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         "",
					},
				},
			},
			response: "OS configuration is missing",
		},
		{
			name:           "Nil OS",
			isPassed:       false,
			isError:        false,
			httpStatusCode: http.StatusGatewayTimeout,
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount:        1,
						AutomateNodeIps:          []string{"127.0.0.1"},
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"1.1.1.1"},
					},
					ExternalOS: nil,
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed, tt.httpStatusResponse)
			defer server.Close()

			svc := NewExternalOpensearchCheck(
				logger.NewLogrusStandardLogger(),
				port,
			)

			tt.args.config.Hardware.AutomateNodeIps = []string{host, host}

			json.Unmarshal([]byte(tt.response), &want)
			for i := range want {
				want[i].Host = host
			}

			got := svc.Run(tt.args.config)

			if tt.isError {
				if tt.name == "Empty OS" {
					assert.Len(t, got, 3)
					assert.NotNil(t, got[0].Result.Error)
					assert.Equal(t, constants.LOCALHOST, got[0].Host)
					assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
					assert.Equal(t, http.StatusBadRequest, got[0].Result.Error.Code)
					assert.Equal(t, tt.response, got[0].Result.Error.Error())
					assert.Equal(t, constants.OS_DETAILS_MISSING, got[0].Result.Error.Message)
					assert.Equal(t, http.StatusBadRequest, got[0].Result.Error.Code)
				} else {
					assert.NotNil(t, got[0].Result.Error)
					assert.Equal(t, constants.LOCALHOST, got[0].Host)
					assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
					assert.Equal(t, tt.httpStatusCode, got[0].Result.Error.Code)
					assert.Equal(t, tt.response, got[0].Result.Error.Error())
				}
			} else {
				if tt.name == "Nil OS" {
					assert.Len(t, got, 3)
					assert.Equal(t, "127.0.0.1", got[0].Host)
					assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
					assert.True(t, got[0].Result.Skipped)
				} else {
					assert.Nil(t, got[0].Result.Error)
					assert.Equal(t, constants.LOCALHOST, got[0].Host)
					assert.Equal(t, constants.AUTOMATE, got[0].NodeType)
					assert.Equal(t, want, got)
				}
			}

		})
	}
}

func TestForChefserverOpensearch(t *testing.T) {
	t.Run("ChefServer Opensearch check pass", func(t *testing.T) {
		var want []models.CheckTriggerResponse

		config := &models.Config{
			Hardware: &models.Hardware{
				ChefInfraServerNodeCount: 2,
			},
			ExternalOS: &models.ExternalOS{
				OSDomainName:   osDomainName,
				OSDomainURL:    osDomainURL,
				OSUsername:     osUsername,
				OSUserPassword: osUserPassword,
				OSCert:         osCert,
				OSRoleArn:      oSRoleArn,
			},
		}
		isError := false
		server, host, port := createDummyServer(t, http.StatusOK, true,"")
		defer server.Close()
		svc := NewExternalOpensearchCheck(
			logger.NewLogrusStandardLogger(),
			port,
		)
		config.Hardware.ChefInfraServerNodeIps = []string{host, host}

		json.Unmarshal([]byte(externalOpensearchResponseSuccessChefServerExpected), &want)
		for i := range want {
			want[i].Host = host
		}

		got := svc.Run(config)
		if isError {
			assert.NotNil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, http.StatusOK, got[0].Result.Error.Code)
			assert.Equal(t, externalOpensearchResponseSuccessChefServerExpected, got[0].Result.Error.Error())
		} else {
			assert.Nil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, want, got)
		}
	})
	t.Run("ChefServer Opensearch check fail", func(t *testing.T) {
		var want []models.CheckTriggerResponse

		config := &models.Config{
			Hardware: &models.Hardware{
				ChefInfraServerNodeCount: 2,
			},
			ExternalOS: &models.ExternalOS{
				OSDomainName:   osDomainName,
				OSDomainURL:    osDomainURL,
				OSUsername:     osUsername,
				OSUserPassword: osUserPassword,
				OSCert:         osCert,
				OSRoleArn:      oSRoleArn,
			},
		}
		isError := false
		server, host, port := createDummyServer(t, http.StatusOK, false,"")
		defer server.Close()
		svc := NewExternalOpensearchCheck(
			logger.NewLogrusStandardLogger(),
			port,
		)
		config.Hardware.ChefInfraServerNodeIps = []string{host, host}

		json.Unmarshal([]byte(externalOpensearchResponseFailureChefServerExpected), &want)
		for i := range want {
			want[i].Host = host
		}

		got := svc.Run(config)
		if isError {
			assert.NotNil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, http.StatusOK, got[0].Result.Error.Code)
			assert.Equal(t, externalOpensearchResponseFailureChefServerExpected, got[0].Result.Error.Error())
		} else {
			assert.Nil(t, got[0].Result.Error)
			assert.Equal(t, constants.LOCALHOST, got[0].Host)
			assert.Equal(t, constants.CHEF_INFRA_SERVER, got[0].NodeType)
			assert.Equal(t, want, got)
		}
	})
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.ExternalOSRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)

			wantReq := getRequest()

			assert.NotNil(t, got)
			assert.Equal(t, got, wantReq)
			if r.URL.Path == constants.EXTERNAL_OPENSEARCH_API_PATH {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(externalOpensearchResponseSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(externalOpensearchResponseFailure))
				}
			}
		}))

		// Extract IP and port from the server's URL
		address := server.URL[strings.Index(server.URL, "//")+2:]
		colonIndex := strings.Index(address, ":")
		ip := address[:colonIndex]
		port := address[colonIndex+1:]

		return server, ip, port
	}

	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(requiredStatusCode)
		w.Write([]byte(requiredStatusResponse))
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewExternalOpensearchCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 0, len(resp))
}

func TestRunCheck(t *testing.T) {
	t.Run("Nil Hardware", func(t *testing.T) {
		config := &models.Config{
			Hardware:   nil,
			ExternalOS: nil,
		}

		newOS := NewExternalOpensearchCheck(logger.NewLogrusStandardLogger(), "8080")
		got := newOS.Run(config)
		assert.Len(t, got, 2)
		assert.Equal(t, constants.UNKNOWN_HOST, got[0].Host)
		assert.Equal(t, constants.CHEF_INFRA_SERVER, got[1].NodeType)
		assert.Equal(t, constants.EXTERNAL_OPENSEARCH, got[1].CheckType)
		assert.True(t, got[0].Result.Skipped)

	})
}
