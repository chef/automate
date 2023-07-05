package fqdnchecktrigger

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

var (
	fqdn                = "www.example.com"
	rootCert            = "rootcert"
	certificateAutomate = []*models.Certificate{{
		Fqdn:         fqdn,
		FqdnRootCert: rootCert,
		Nodes:        nil,
		NodeType:     constants.AUTOMATE,
	},
	}

	certificateChefServer = []*models.Certificate{{
		Fqdn:         fqdn,
		FqdnRootCert: rootCert,
		Nodes:        nil,
		NodeType:     constants.CHEF_INFRA_SERVER,
	},
	}
)

const (
	token              = "test-token"
	fqnResponseSuccess = `{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "FQDN is reachable",
			  "passed": true,
			  "success_msg": "FQDN is reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Nodes are reachable",
			  "passed": true,
			  "success_msg": "All nodes are reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificate validity for FQDN",
			  "passed": true,
			  "success_msg": "FQDN has with valid certificates",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`

	fqnResponseFailure = `
	  {
		"status": "SUCCESS",
		"result": {
		  "passed": false,
		  "checks": [
			{
			  "title": "FQDN is reachable",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "FQDN is not reachable",
			  "resolution_msg": "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
			},
			{
			  "title": "Nodes are reachable",
			  "passed": false,
			  "success_msg": "All nodes are reachable",
			  "error_msg": "1.2.3.4 is not reachable",
			  "resolution_msg": "Ensure your Port 443 is open. Review security group or firewall settings."
			},
			{
			  "title": "Certificate validity for FQDN",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "FQDN certificate is not valid, <error_reason>",
			  "resolution_msg": "Generate new valid certificates and provide those."
			}
		  ]
		}
	  }
	  `

	fqdnTriggerResponseAutomateSuccess = `[
		{
		"host": "1.2.3.4",
		"node_type" :"automate",
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "FQDN is reachable",
			  "passed": true,
			  "success_msg": "FQDN is reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Nodes are reachable",
			  "passed": true,
			  "success_msg": "All nodes are reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificate validity for FQDN",
			  "passed": true,
			  "success_msg": "FQDN has with valid certificates",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }
	  ]`

	fqdnTriggerResponseChefServerSuccess = `[
		{
		"host": "5.6.7.8",
		"node_type" :"chef-infra-server",
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "FQDN is reachable",
			  "passed": true,
			  "success_msg": "FQDN is reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Nodes are reachable",
			  "passed": true,
			  "success_msg": "All nodes are reachable",
			  "error_msg": "",
			  "resolution_msg": ""
			},
			{
			  "title": "Certificate validity for FQDN",
			  "passed": true,
			  "success_msg": "FQDN has with valid certificates",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }
	  ]`

	fqdnTriggerResponseAutomateFailure = `[
		{
			"status": "SUCCESS",
			"host": "1.2.3.4",
			"node_type": "automate",
			"result": {
				"passed": false,
				"checks": [
					{
						"title": "FQDN is reachable",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN is not reachable",
						"resolution_msg": "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
					},
					{
						"title": "Nodes are reachable",
						"passed": false,
						"success_msg": "All nodes are reachable",
						"error_msg": "1.2.3.4 is not reachable",
						"resolution_msg": "Ensure your Port 443 is open. Review security group or firewall settings."
					},
					{
						"title": "Certificate validity for FQDN",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN certificate is not valid, <error_reason>",
						"resolution_msg": "Generate new valid certificates and provide those."
					}
				]
			}
		}
	]`
	fqdnTriggerResponseChefServerFailure = `[
		{
			"status": "SUCCESS",
			"host": "5.6.7.8",
		"node_type" :"chef-infra-server",
			"result": {
				"passed": false,
				"checks": [
					{
						"title": "FQDN is reachable",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN is not reachable",
						"resolution_msg": "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
					},
					{
						"title": "Nodes are reachable",
						"passed": false,
						"success_msg": "All nodes are reachable",
						"error_msg": "1.2.3.4 is not reachable",
						"resolution_msg": "Ensure your Port 443 is open. Review security group or firewall settings."
					},
					{
						"title": "Certificate validity for FQDN",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN certificate is not valid, <error_reason>",
						"resolution_msg": "Generate new valid certificates and provide those."
					}
				]
			}
		}
	]`

	fqdnTriggerResponseChefServerForMoreThanOneNodeFailure = `[
		{
			"status": "SUCCESS",
			"host": "5.6.7.8",
		"node_type" :"chef-infra-server",
			"result": {
				"passed": false,
				"checks": [
					{
						"title": "FQDN is reachable",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN is not reachable",
						"resolution_msg": "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
					},
					{
						"title": "Nodes are reachable",
						"passed": false,
						"success_msg": "All nodes are reachable",
						"error_msg": "1.2.3.4 is not reachable",
						"resolution_msg": "Ensure your Port 443 is open. Review security group or firewall settings."
					},
					{
						"title": "Certificate validity for FQDN",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN certificate is not valid, <error_reason>",
						"resolution_msg": "Generate new valid certificates and provide those."
					}
				]
			}
		},
		{
			"status": "SUCCESS",
			"host": "5.6.7.8",
		"node_type" :"chef-infra-server",
			"result": {
				"passed": false,
				"checks": [
					{
						"title": "FQDN is reachable",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN is not reachable",
						"resolution_msg": "Ensure your Port 443 is open and load balancer is able to reach to the machine on port 443. Review security group or firewall settings."
					},
					{
						"title": "Nodes are reachable",
						"passed": false,
						"success_msg": "All nodes are reachable",
						"error_msg": "1.2.3.4 is not reachable",
						"resolution_msg": "Ensure your Port 443 is open. Review security group or firewall settings."
					},
					{
						"title": "Certificate validity for FQDN",
						"passed": false,
						"success_msg": "",
						"error_msg": "FQDN certificate is not valid, <error_reason>",
						"resolution_msg": "Generate new valid certificates and provide those."
					}
				]
			}
		}
	]`
)

func TestFqdnCheck_Run(t *testing.T) {

	type args struct {
		config *models.Config
	}
	tests := []struct {
		name                   string
		args                   args
		isPassed               bool
		response               string
		httpStatus             int
		isError                bool
		requiredStatusResponse string
	}{
		{
			name: "Passed Automate Node Check",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate: certificateAutomate,
					APIToken:    token,
				},
			},
			isPassed:   true,
			response:   fqdnTriggerResponseAutomateSuccess,
			httpStatus: http.StatusOK,
			isError:    false,
		},
		{
			name: "Failure Check Automate Nodes",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate: certificateAutomate,
					APIToken:    token,
				},
			},
			isPassed:   false,
			response:   fqdnTriggerResponseAutomateFailure,
			httpStatus: http.StatusOK,
			isError:    false,
		},
		{
			name: "Failure Chef server fqn node Nodes",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: certificateChefServer,
					APIToken:    "test-token",
				},
			},
			isPassed:   false,
			response:   fqdnTriggerResponseChefServerFailure,
			httpStatus: http.StatusOK,
			isError:    false,
		},
		{
			name: "Passed Chef server Node Check",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: certificateChefServer,
					APIToken:    token,
				},
			},
			isPassed:   true,
			response:   fqdnTriggerResponseChefServerSuccess,
			httpStatus: http.StatusOK,
			isError:    false,
		},
		{
			name: "Failed Chef server Node Check for more than one node",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8", "5.6.7.8"},
					},
					Certificate: certificateChefServer,
					APIToken:    token,
				},
			},
			isPassed:   false,
			response:   fqdnTriggerResponseChefServerForMoreThanOneNodeFailure,
			httpStatus: http.StatusOK,
			isError:    false,
		},
		{
			name: "Couldn't call API for chef server nodes",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: certificateChefServer,
					APIToken:    token,
				},
			},
			isPassed:   false,
			response:   "Internal Server Error",
			httpStatus: http.StatusInternalServerError,
			isError:    true,
			requiredStatusResponse: `{"error":{"code":500,"message":"Internal Server Error"}}`,
		},
		{
			name: "400 Bad Reqest",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: certificateChefServer,
					APIToken:    token,
				},
			},
			isPassed:   false,
			response:   "fqdn, root_cert and nodes can't be empty, Please provide all the required fields.",
			httpStatus: http.StatusBadRequest,
			isError:    true,
			requiredStatusResponse: `{"error":{"code":400,"message":"fqdn, root_cert and nodes can't be empty, Please provide all the required fields."}}`,
		},
		{
			name: "Passed Automate Node Check Post Deployment",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate:     certificateAutomate,
					DeploymentState: "post-deploy",
					APIToken:        token,
				},
			},
			isPassed:   true,
			response:   fqdnTriggerResponseAutomateSuccess,
			httpStatus: http.StatusOK,
			isError:    false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var want []models.CheckTriggerResponse
			json.Unmarshal([]byte(tt.response), &want)

			server, host, port := createDummyServer(t, tt.httpStatus, tt.isPassed, tt.requiredStatusResponse)
			defer server.Close()

			fqc := NewFqdnCheck(

				logger.NewLogrusStandardLogger(), port,
			)

			fqc.host = host
			got := fqc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, tt.args.config.Hardware.ChefInfraServerNodeCount)
				assert.NotNil(t, got[0].Result.Error)
				assert.Equal(t, "chef-infra-server", got[0].NodeType)
				assert.Equal(t, got[0].Result.Error.Code, tt.httpStatus)
				assert.Equal(t, tt.response, got[0].Result.Error.Error())
			} else {
				assert.Equal(t, got, want)
				assert.NotNil(t, got)
				assert.Nil(t, got[0].Result.Error)
			}

		})
	}
}

func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			var got models.FqdnRequest
			req := r.Body
			reader, _ := io.ReadAll(req)
			json.Unmarshal(reader, &got)

			wantReq := getRequest()

			assert.NotNil(t, got)
			assert.Equal(t, got.RootCert, wantReq.RootCert)
			assert.NotNil(t, got.Nodes)
			assert.Equal(t, got.Fqdn, wantReq.Fqdn)

			if r.URL.Path == constants.FQDN_LOAD_BALANCER_CHECK {
				if isPassed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(fqnResponseSuccess))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(fqnResponseFailure))
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

func getRequest() models.FqdnRequest {

	return models.FqdnRequest{
		Fqdn:     fqdn,
		RootCert: rootCert,
	}
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewFqdnCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 2, len(resp))
	assert.Equal(t, 1, len(resp["automate"]["https"]))
	assert.Equal(t, 1, len(resp["chef-infra-server"]["https"]))
	assert.Equal(t, true, true)
}
