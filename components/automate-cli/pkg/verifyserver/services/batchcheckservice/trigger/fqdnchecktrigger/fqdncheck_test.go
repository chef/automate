package fqdnchecktrigger

import (
	"encoding/json"
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
		config models.Config
	}
	tests := []struct {
		name       string
		args       args
		isPassed   bool
		response   string
		httpStatus int
		isError    bool
	}{
		{
			name: "Passed Automate Node Check",
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
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
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
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
				config: models.Config{
					Hardware: models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
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
				config: models.Config{
					Hardware: models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
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
				config: models.Config{
					Hardware: models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8", "5.6.7.8"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
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
				config: models.Config{
					Hardware: models.Hardware{
						ChefInfraServerNodeCount: 1,
						ChefInfraServerNodeIps:   []string{"5.6.7.8"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
				},
			},
			isPassed:   false,
			response:   "error while connecting to the endpoint, received invalid status code",
			httpStatus: http.StatusInternalServerError,
			isError:    true,
		},
		{
			name: "Passed Automate Node Check Post Deployment",
			args: args{
				config: models.Config{
					Hardware: models.Hardware{
						AutomateNodeCount: 1,
						AutomateNodeIps:   []string{"1.2.3.4"},
					},
					Certificate: models.Certificate{
						AutomateFqdn: "www.example.com",
						RootCert:     "rootcert",
					},
					DeploymentState: "post-deploy",
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

			server, host, port := createDummyServer(t, tt.httpStatus, tt.isPassed)
			defer server.Close()

			fqc := &FqdnCheck{
				port: port,
				log:  logger.NewLogrusStandardLogger(),
				host: host,
			}

			got := fqc.Run(tt.args.config)

			if tt.isError {
				assert.Len(t, got, tt.args.config.Hardware.ChefInfraServerNodeCount)
				assert.NotNil(t, got[0].Error)
				assert.Equal(t, "chef-infra-server", got[0].NodeType)
				assert.Equal(t, got[0].Error.Code, tt.httpStatus)
				assert.Equal(t, tt.response, got[0].Error.Error())
			} else {
				assert.Equal(t, got, want)
				assert.NotNil(t, got)
				assert.Nil(t, got[0].Error)
			}

		})
	}
}

func createDummyServer(t *testing.T, requiredStatusCode int, isPassed bool) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
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
	}))

	// Extract IP and port from the server's URL
	address := server.URL[strings.Index(server.URL, "//")+2:]
	colonIndex := strings.Index(address, ":")
	ip := address[:colonIndex]
	port := address[colonIndex+1:]

	return server, ip, port
}
