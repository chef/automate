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

	externalOpensearchResponseSuccessExpected = `[
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
		}
		]
	  `
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
	externalOpensearchResponseFailureExpected = `[
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
		}
	]`

	osDomainName   = "opensearch_managed"
	osDomainURL    = "https://opensearchdomain.com"
	osUsername     = "admin"
	osUserPassword = "Chefautomate"
	osCert         = ""
)

func getRequest() models.ExternalOS {
	return models.ExternalOS{
		OSDomainName:   osDomainName,
		OSDomainURL:    osDomainURL,
		OSUsername:     osUsername,
		OSUserPassword: osUserPassword,
		OSCert:         osCert,
	}

}

func TestOpensearchCheck_Run(t *testing.T) {
	type args struct {
		config models.Config
	}

	tests := []struct {
		name           string
		isPassed       bool
		args           args
		response       string
		httpStatusCode int
		isError        bool
	}{
		{
			name:           "Opensearch Checks are passed",
			isPassed:       true,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount:        2,
						ChefInfraServerNodeCount: 2,
					},
					ExternalOS: models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
					},
				},
			},
			response: externalOpensearchResponseSuccessExpected,
		},

		{
			name:           "Postgres checks are failed",
			isPassed:       false,
			isError:        false,
			httpStatusCode: http.StatusOK,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount:        2,
						ChefInfraServerNodeCount: 2,
					},
					ExternalOS: models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
					},
				},
			},
			response: externalOpensearchResponseFailureExpected,
		},
		{
			name:           "Internal Server Error",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusInternalServerError,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount:        2,
						ChefInfraServerNodeCount: 2,
					},
					ExternalOS: models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
					},
				},
			},
			response: "error while connecting to the endpoint, received invalid status code",
		},
		{
			name:           "Gateway Timeout",
			isPassed:       false,
			isError:        true,
			httpStatusCode: http.StatusGatewayTimeout,
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount:        2,
						ChefInfraServerNodeCount: 2,
					},
					ExternalOS: models.ExternalOS{
						OSDomainName:   osDomainName,
						OSDomainURL:    osDomainURL,
						OSUsername:     osUsername,
						OSUserPassword: osUserPassword,
						OSCert:         osCert,
					},
				},
			},
			response: "error while connecting to the endpoint, received invalid status code",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			server, host, port := createDummyServer(t, tt.httpStatusCode, tt.isPassed)
			defer server.Close()

			svc := NewExternalOpensearchCheck(
				logger.NewLogrusStandardLogger(),
				port,
			)

			tt.args.config.Hardware.AutomateNodeIps = []string{host, host}
			tt.args.config.Hardware.ChefInfraServerNodeIps = []string{host, host}

			json.Unmarshal([]byte(tt.response), &want)
			for i := range want {
				want[i].Host = host
			}

			got := svc.Run(tt.args.config)

			if tt.isError {
				assert.NotNil(t, got[0].Result.Error)
				assert.Equal(t, got[0].Result.Error.Code, tt.httpStatusCode)
				assert.Equal(t, tt.response, got[0].Result.Error.Error())
			} else {
				assert.Equal(t, want, got)
			}

		})
	}
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.ExternalOS
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
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}
