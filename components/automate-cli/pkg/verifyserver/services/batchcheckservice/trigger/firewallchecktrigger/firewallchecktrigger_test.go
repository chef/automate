package firewallchecktrigger

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const (
	firewallCheckResp1 = `{
		"status": "SUCCESS",
		"result": {
		  "passed": true,
		  "checks": [
			{
			  "title": "Check for reachability of service at destination port",
			  "passed": false,
			  "success_msg": "",
			  "error_msg": "The <protocol> service running at <destination_node_ip>:<destination_node_port> is not reachable from <source_ip>",
			  "resolution_msg": "Check your firewall settings to provide access to <destination_node_port> port at <destination_node_ip> from <source_node_ip>"
			}
		  ]
		}
	  }`
)

func TestFirewallCheck_Run(t *testing.T) {

	t.Run("All checks passes", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusOK)
		defer server.Close()

		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:        []string{host},
				PostgresqlNodeIps:      []string{host},
				OpenSearchNodeIps:      []string{host},
				ChefInfraServerNodeIps: []string{host},
			},
			Certificate: models.Certificate{
				Nodes: []models.NodeCert{
					{
						Cert: "cert",
						Key:  "key",
					},
				},
				RootCert: "root_cert",
			},
		}
		fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), port)
		resp := fwc.Run(config)
		require.NotNil(t, resp)
		require.Len(t, resp, 30)

		for _, rep := range resp {
			if rep.NodeType == constants.BASTION {
				require.Empty(t, rep.Status)
				require.Empty(t, rep.Result)
				require.Equal(t, rep.Host, "127.0.0.1")
				require.Equal(t, http.StatusInternalServerError, rep.Error.Code)
				require.Equal(t, "error while parsing the response data:EOF", rep.Error.Message)
			}
			if rep.NodeType == constants.AUTOMATE {
				require.NotEmpty(t, rep.Status)
				require.NotEmpty(t, rep.Result)
				require.Equal(t, rep.Host, "127.0.0.1")
				require.Equal(t, "Check for reachability of service at destination port", rep.Result.Checks[0].Title)
				require.False(t, rep.Result.Checks[0].Passed)
				require.True(t, rep.Result.Passed)
			}
		}
	})

	t.Run("Failed Firewall Check", func(t *testing.T) {
		// Create a dummy server
		server, host, port := createDummyServer(t, http.StatusInternalServerError)
		defer server.Close()

		// Test data
		config := models.Config{
			Hardware: models.Hardware{
				AutomateNodeIps:        []string{host},
				PostgresqlNodeIps:      []string{host},
				OpenSearchNodeIps:      []string{host},
				ChefInfraServerNodeIps: []string{host},
			},
			Certificate: models.Certificate{
				Nodes: []models.NodeCert{
					{
						Cert: "cert",
						Key:  "key",
					},
				},
				RootCert: "root_cert",
			},
		}
		fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), port)
		ctr := fwc.Run(config)

		require.Len(t, ctr, 30)
		require.NotNil(t, ctr[0].Error)
		require.Equal(t, ctr[0].Error.Code, http.StatusInternalServerError)
		assert.Equal(t, "error while connecting to the endpoint, received invalid status code", ctr[0].Error.Error())
	})
}

func TestMakeRequests(t *testing.T) {
	// Create a sample configuration
	config := models.Config{
		Hardware: models.Hardware{
			AutomateNodeIps:        []string{"10.0.0.1", "10.0.0.2"},
			PostgresqlNodeIps:      []string{"10.0.0.3", "10.0.0.4"},
			OpenSearchNodeIps:      []string{"10.0.0.5", "10.0.0.6"},
			ChefInfraServerNodeIps: []string{"10.0.0.7", "10.0.0.8"},
		},
		Certificate: models.Certificate{
			Nodes: []models.NodeCert{
				{
					Cert: "cert",
					Key:  "key",
				},
			},
			RootCert: "root_cert",
		},
	}

	// Define the expected result
	expected := []trigger.ReqBody{
		// Expected requests for Chef Automate to all the OS and PG nodes
		{
			SourceNodeIP:               "10.0.0.1",
			DestinationNodeIP:          "10.0.0.3",
			DestinationServicePort:     "7432",
			DestinationServiceProtocol: "tcp",
			Cert:                       "cert",
			Key:                        "key",
			RootCert:                   "root_cert",
			NodeType:                   constants.AUTOMATE,
		},
		// More expected requests...
	}

	// Call the function
	result := makeRequests(config)

	require.Len(t, result, 92)
	require.Equal(t, expected[0].SourceNodeIP, result[0].SourceNodeIP)
	require.Equal(t, expected[0].DestinationNodeIP, result[0].DestinationNodeIP)
	require.Equal(t, "automate", result[0].NodeType)
}

// Helper function to create a dummy server
func createDummyServer(t *testing.T, requiredStatusCode int) (*httptest.Server, string, string) {
	if requiredStatusCode == http.StatusOK {
		server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.URL.Path == constants.FIREWALL_API_PATH {
				body, err := ioutil.ReadAll(r.Body)
				require.NoError(t, err)

				var req trigger.ReqBody

				err = json.Unmarshal(body, &req)
				require.NoError(t, err)
				require.NotZero(t, req)
				require.NotEmpty(t, req)

				if req.NodeType == constants.AUTOMATE {
					w.WriteHeader(http.StatusOK)
					w.Write([]byte(firewallCheckResp1))
				} else {
					w.WriteHeader(http.StatusOK)
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
