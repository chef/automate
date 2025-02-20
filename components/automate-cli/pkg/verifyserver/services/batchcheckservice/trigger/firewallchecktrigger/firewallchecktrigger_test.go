package firewallchecktrigger

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
	"github.com/stretchr/testify/require"
)

var (
	hardware = &models.Hardware{
		AutomateNodeIps:        []string{"10.0.0.1", "10.0.0.2"},
		PostgresqlNodeIps:      []string{"10.0.0.3", "10.0.0.6"},
		OpenSearchNodeIps:      []string{"10.0.0.5", "10.0.0.10"},
		ChefInfraServerNodeIps: []string{"10.0.0.7"},
	}

	certificatelist = []*models.Certificate{
		{
			Nodes: nodes,
		},
	}

	nodeCert = &models.NodeCert{
		IP:  "10.0.0.1",
		Key: "test-key",
	}

	nodes = []*models.NodeCert{nodeCert}
)

const (
	automatepgTcpPass = `{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Check for reachability of service at destination port from the source node",
			  "passed": true,
			  "success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
			  "error_msg": "",
			  "resolution_msg": ""
			}
		  ]
		}
	  }`

	automatePgFailureResponse = `
	{
			"status": "SUCCESS",
			"result": {
			  "passed": false,
			  "checks": [
				{
				  "title": "Check for reachability of service at destination port",
				  "passed": false,
				  "success_msg": "",
				  "error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
				  "resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>"
				}
			  ]
			}
		  }
	`
	apiTriggerAutomatePgPass = `[
		{
			"status": "SUCCESS",
			"host": "10.0.0.1",
			"node_type": "automate",
			"check_type": "",
			"result": {
			  "passed": true,
			  "msg": "",
			  "check": "",
			  "checks": [
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				},
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				},
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				},
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				},
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				},
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
					"resolution_msg": "",
				  	"skipped": false
				}
			  ],
			  "skipped": false
			}
		  }
	]
	 `

	apiTriggerFailureResponse = `[
		{
			"status": "SUCCESS",
			"host": "10.0.0.1",
			"node_type": "automate",
			"check_type": "",
			"result": {
				"passed": false,
				"msg": "",
				"check": "",
				"checks": [
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					},
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					},
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					},
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					},
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					},
					{
						"title": "Check for reachability of service at destination port",
						"passed": false,
						"success_msg": "",
						"error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
						"resolution_msg": "Check your firewall settings and provide access to port <destination_node_port> on <destination_node_ip> from <source_node_ip>",
						"skipped": false
					}
				],
				"skipped": false
			}
		}
	]`

	apiTriggerInvalidParsedResponse = `{
		"status": "SUCCESS",
		"host": "10.0.0.1",
		"node_type": "automate",
		"result": {
			"passed": true,
			"checks": [
				{
					"title": "Check for reachability of service at destination port from the source node",
					"passed": true,
					"success_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is reachable from <source_node_ip>",
					"error_msg": "",
		
	 
	 `
)

func TestMakeRequests(t *testing.T) {
	// Create a sample configuration
	config := &models.Config{
		Hardware:    hardware,
		Certificate: certificatelist,
	}

	// Define the expected result
	expected := []models.FirewallRequest{
		// Expected requests for Chef Automate to all the OS and PG nodes
		{
			SourceNodeIP:               "10.0.0.1",
			DestinationNodeIP:          "10.0.0.3",
			DestinationServicePort:     "7432",
			DestinationServiceProtocol: "tcp",
		},
		{
			SourceNodeIP:               "10.0.0.1",
			DestinationNodeIP:          "10.0.0.5",
			DestinationServicePort:     "7432",
			DestinationServiceProtocol: "tcp",
		},
		{
			SourceNodeIP:               "10.0.0.1",
			DestinationNodeIP:          "10.0.0.10",
			DestinationServicePort:     "9200",
			DestinationServiceProtocol: "tcp",
		},
		{
			SourceNodeIP:               "10.0.0.1",
			DestinationNodeIP:          "10.0.0.6",
			DestinationServicePort:     "9200",
			DestinationServiceProtocol: "tcp",
		},
	}

	mapRequests := make(map[string][]models.FirewallRequest)
	// Call the function
	makeRequests(config, mapRequests)
	requestsForautomate, ok := mapRequests[constants.AUTOMATE]
	assert.True(t, ok)
	assert.Equal(t, 24, len(requestsForautomate))

	requestsForchefServer, ok := mapRequests[constants.CHEF_INFRA_SERVER]
	assert.True(t, ok)
	assert.Equal(t, 12, len(requestsForchefServer))

	//As there are two postgress nodes which needs to interact with eachother
	requestsForPostgres, ok := mapRequests[constants.POSTGRESQL]
	assert.True(t, ok)
	assert.Equal(t, len(requestsForPostgres), 10)

	requestsForOpensearch, ok := mapRequests[constants.OPENSEARCH]
	assert.True(t, ok)
	assert.Equal(t, len(requestsForOpensearch), 8)

	require.Equal(t, expected[0].SourceNodeIP, requestsForautomate[0].SourceNodeIP)
	require.Equal(t, expected[0].DestinationNodeIP, requestsForautomate[0].DestinationNodeIP)
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int, invalidParseResponse bool, isFailed bool, requiredStatusResponse string) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.URL.Path == constants.FIREWALL_API_PATH {
				body, err := io.ReadAll(r.Body)
				require.NoError(t, err)

				var req models.FirewallRequest

				err = json.Unmarshal(body, &req)
				require.NoError(t, err)
				require.NotZero(t, req)
				require.NotEmpty(t, req)
				if invalidParseResponse {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(apiTriggerInvalidParsedResponse))

				} else if isFailed {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(automatePgFailureResponse))
				} else {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(automatepgTcpPass))
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

func TestFirewallCheck_Run(t *testing.T) {

	type args struct {
		config *models.Config
	}
	tests := []struct {
		name                   string
		args                   args
		response               string
		httpStatusCode         int
		isError                bool
		invalidParseResponse   bool
		isFailed               bool
		requiredStatusResponse string
	}{
		{
			name: "Automate Pg Passed",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeIps:   []string{"10.0.0.1"},
						PostgresqlNodeIps: []string{"10.0.0.3"},
						OpenSearchNodeIps: []string{"10.0.0.5"},
					},
					Certificate: certificatelist,
				},
			},
			response:       apiTriggerAutomatePgPass,
			httpStatusCode: http.StatusOK,
		},
		{
			name: "Automate Pg failure",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeIps:   []string{"10.0.0.1"},
						PostgresqlNodeIps: []string{"10.0.0.3"},
						OpenSearchNodeIps: []string{"10.0.0.5"},
					},
					Certificate: certificatelist,
				},
			},
			isFailed:       true,
			response:       apiTriggerFailureResponse,
			httpStatusCode: http.StatusOK,
		},
		{
			name: "Internal Server Error For automate and other nodes",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeIps:   []string{"10.0.0.1"},
						PostgresqlNodeIps: []string{"10.0.0.3"},
						OpenSearchNodeIps: []string{"10.0.0.5"},
					},
					Certificate: certificatelist,
				},
			},
			response:               "Internal Server Error",
			httpStatusCode:         http.StatusInternalServerError,
			isError:                true,
			invalidParseResponse:   false,
			requiredStatusResponse: `{"error":{"code":500,"message":"Internal Server Error"}}`,
		},
		{
			name: "Error in parsing the response",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeIps:   []string{"10.0.0.1"},
						PostgresqlNodeIps: []string{"10.0.0.3"},
						OpenSearchNodeIps: []string{"10.0.0.5"},
					},
					Certificate: certificatelist,
				},
			},
			response:             "error while parsing the response data",
			httpStatusCode:       http.StatusOK,
			isError:              true,
			invalidParseResponse: true,
		},
		{
			name: "Status Bad Request",
			args: args{
				config: &models.Config{
					Hardware: &models.Hardware{
						AutomateNodeIps:   []string{"10.0.0.1"},
						PostgresqlNodeIps: []string{"10.0.0.3"},
						OpenSearchNodeIps: []string{"10.0.0.5"},
					},
					Certificate: certificatelist,
				},
			},
			response:               "source_node_ip, destination_node_ip, destination_service_port or destination_service_protocol cannot be empty",
			httpStatusCode:         http.StatusBadRequest,
			isError:                true,
			invalidParseResponse:   true,
			requiredStatusResponse: `{"error":{"code":400,"message":"source_node_ip, destination_node_ip, destination_service_port or destination_service_protocol cannot be empty"}}`,
		},
		{
			name: "Hardware Nil",
			args: args{
				config: &models.Config{
					Hardware: nil,
					Certificate: []*models.Certificate{
						{
							Nodes: nodes,
						},
					},
					ExternalOS: nil,
					ExternalPG: nil,
				},
			},
			response:       "",
			httpStatusCode: http.StatusOK,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Run(tt.name, func(t *testing.T) {

				// Create a dummy server
				server, host, port := createDummyServer(t, tt.httpStatusCode, tt.invalidParseResponse, tt.isFailed, tt.requiredStatusResponse)
				defer server.Close()

				fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), port)
				fwc.host = host
				got := fwc.Run(tt.args.config)

				if tt.isError {
					assert.NotNil(t, got[0].Result.Error)
					assert.Contains(t, got[0].Result.Error.Error(), tt.response)
					if tt.httpStatusCode == http.StatusBadRequest {
						assert.Equal(t, got[0].Result.Error.Code, tt.httpStatusCode)
					} else {
						assert.Equal(t, got[0].Result.Error.Code, http.StatusInternalServerError)
					}
					for i := range got {
						assert.NotNil(t, got[i].NodeType)
						assert.NotNil(t, got[i].Host)
					}

				} else if tt.name == "Hardware Nil" {
					assert.Len(t, got, 5)
					for _, v := range got {
						assert.Equal(t, constants.FIREWALL, v.CheckType)
						assert.Equal(t, constants.FIREWALL, v.Result.Check)
						assert.True(t, v.Result.Skipped)
					}
				} else {
					var want []models.CheckTriggerResponse
					json.Unmarshal([]byte(tt.response), &want)
					assert.Equal(t, len(got), len(want))
					for i := range want {
						//Checking each want trigger response is present in what we go in case of non-error
						assert.Contains(t, got, want[i])
					}
				}

			})

		})

	}
}

func TestGetPortsForMockServer(t *testing.T) {
	fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), "1234")
	resp := fwc.GetPortsForMockServer()

	assert.Equal(t, 4, len(resp))
	assert.Equal(t, 4, len(resp["postgresql"]["tcp"]))
	assert.Equal(t, 1, len(resp["postgresql"]["udp"]))
	assert.Equal(t, 3, len(resp["opensearch"]["tcp"]))
	assert.Equal(t, true, true)
}
