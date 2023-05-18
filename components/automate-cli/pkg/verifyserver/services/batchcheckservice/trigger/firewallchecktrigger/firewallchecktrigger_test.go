package firewallchecktrigger

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/require"
)

func TestFirewallCheck_Run(t *testing.T) {

	t.Run("All checks passes", func(t *testing.T) {
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
		fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), "")
		resp := fwc.Run(config)
		require.NotNil(t, resp)
		// TODO: Make the all checks passed also write for the failed scenario
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
